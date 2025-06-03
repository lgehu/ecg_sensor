with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Bounded;
with Interfaces; use Interfaces;

with HAL; use HAL;
with HAL.UART; use HAL.UART;

with UART_USB;
with PanTompkins;
with AdaData;

package body Ecg_Sensor is

   -- TODO: Add parameter for input channel and output channel selection
   -- TODO: Command to acquire an arbitrary ammount of sample (ACQUIRE=100)
   -- TODO: Resolve bug of overflow from the UART 

   ECG_VERSION : constant String := "0.1";
   CR_LF : constant String := ASCII.CR & ASCII.LF;

   END_CMD_FLAG : constant Character := ASCII.Semicolon;
   CMD_END : constant String := END_CMD_FLAG & "";

   type Sampling_Mode is (Mode_Loop, Mode_Onetime);
   type Sensor_State_Type is (INIT, SAMPLING, PAUSED);

   type Output_Format_Type is (OUT_ASCII, IEEE_FLOAT_32_BIGE);

   package UART_STR renames UART_USB.B_Str;

   Sample_Index : Positive := 1;                -- Current index in the sample data 
   Raw_Input : UART_USB.UART_String;            -- Buffer to store incoming commands
   Current_State : Sensor_State_Type := INIT;   -- Current state of the sensor
   
   procedure Log (Msg : String) renames UART_USB.Transmit_String;

   procedure Return_Arg (Arg : Commands_Interpreter.Argument; Valid : Boolean) is
   begin
      if Valid then
         Log ("OK" & CMD_END);
      else
         Log ("Invalid parameter" & CMD_END);
      end if;
   end;

   procedure Print_Args (Input : Commands_Interpreter.Argument; Valid : Boolean) is
   Args : Commands_Interpreter.Arg_Array (1 .. Commands_Interpreter.Get_Arg_Count);
   begin
      Commands_Interpreter.Get_Args (Args);
      for I in Args'Range loop
         Log (Args (I).Key'Image & " = " & Args (I).Value'Image & CR_LF);
      end loop;
      Log(CMD_END);
   end Print_Args;

   package Sample_Rate is new Commands_Interpreter.Real_Accessor (T => PanTompkins.Sampling_Frequency_Type,
               Key            => "SAMPLE_RATE",
               Default_Value  => 100.0,
               Action_Fn      => Return_Arg'Access
            );

   package Amplitude_Coef is new Commands_Interpreter.Real_Accessor (T => PanTompkins.Amplitude_Treshold_Coef_Type,
               Key            => "AMPLITUDE_COEF",
               Default_Value  => 1.5,
               Action_Fn      => Return_Arg'Access
            );

   package Pick_Distance is new Commands_Interpreter.Real_Accessor (T => PanTompkins.Positive_Float,
               Key            => "PICK_DISTANCE",
               Default_Value  => 0.3,
               Action_Fn      => Return_Arg'Access
            );

   package Window_Sec is new Commands_Interpreter.Real_Accessor (T => PanTompkins.Positive_Float,
               Key            => "WINDOW_SEC",
               Default_Value  => 0.4,
               Action_Fn      => Return_Arg'Access
            );

   package Output_Stage is new Commands_Interpreter.Discrete_Accessor (T => PanTompkins.Stage,
               Key            => "OUTPUT_STAGE",
               Default_Value  => PanTompkins.Stage_Integrated,
               Action_Fn      => Return_Arg'Access
            );

   package Set_State is new Commands_Interpreter.Arg_Accessor (T => Sensor_State_Type,
               Key => "SET_STATE",
               Default_Value => INIT,
               Is_Valid => Valide_State'Access,
               Do_Action => null);

   package Get_Args is new Commands_Interpreter.Action_Accessor (
               Key => "GET_ARGS", 
               Action_Fn => Print_Args'Access);

   package Output_Format is new Commands_Interpreter.Discrete_Accessor (T => Output_Format_Type,
               Key => "OUTPUT_FORMAT", 
               Default_Value => IEEE_FLOAT_32_BIGE,
               Action_Fn => Return_Arg'Access
            );

   procedure Read_UART is
   Status : UART_Status;
   Char : Character;
   Arg : Commands_Interpreter.Argument;
   Data : UInt9;
   begin
      UART_USB.Read_Blocking (Data, Status, Milliseconds (0));
      if Status = HAL.UART.Ok then
         Char := Character'Val (Data);
         if UART_STR.Length (Raw_Input) >= UART_STR.Max_Length or Char = END_CMD_FLAG then
            
            begin
               Arg := Commands_Interpreter.Parse (UART_STR.To_String (Raw_Input));
            exception
               when E : Commands_Interpreter.Commands_Exception =>
                  Log (Exception_Message (E) & CR_LF);
            end;

            Raw_Input := UART_STR.Null_Bounded_String;
         else
            UART_STR.Append (Raw_Input, Char);
         end if;
      end if;
   end Read_UART;

   function Valide_State (Input : Commands_Interpreter.Argument) return Boolean is
   Intended_State : Sensor_State_Type;
   Old_State : Sensor_State_Type := Current_State;
   begin

      begin
         Intended_State := Sensor_State_Type'Value (
                              Commands_Interpreter.Command_String.To_String (Input.Value));
         
         case Intended_State is
            when SAMPLING =>
               if Current_State = INIT then
                  Current_State := SAMPLING;

                  PanTompkins.Initialize ((Sampling_Frequency => Sample_Rate.Get_Value, 
                                          Amplitude_Treshold_Coef => Amplitude_Coef.Get_Value,
                                          Minimal_Pick_Distance_Sec => Pick_Distance.Get_Value, 
                                          Window_Sec => Window_Sec.Get_Value, 
                                          Output_Stage => Output_Stage.Get_Value));

               elsif Current_State = PAUSED then
                  Current_State := SAMPLING;
               end if;
            when INIT =>
               if Current_State = SAMPLING or 
                     Current_State = PAUSED or 
                     Current_State = INIT then
                  Current_State := INIT;
               end if;
            when PAUSED =>
               if Current_State = SAMPLING then
                  Current_State := PAUSED;
               end if;
         end case;

         if Intended_State /= Current_State then
            Log ("Can't change state from " & 
                  Current_State'Image & " to " & Intended_State'Image & CR_LF & CMD_END);
            return False;
         else
            Log ("OK" & CMD_END);
         end if;


      exception
         when E : Constraint_Error =>
            Log (Exception_Message (E) & CMD_END);
            return False;
      end;

      return True;

   end Valide_State;

   procedure Initialize is
   begin
      UART_USB.Initialize (115_200);
      UART_USB.Transmit_String ("ECG_SENSOR v0.1" & CR_LF);

      Amplitude_Coef.Register;
      Sample_Rate.Register;
      Pick_Distance.Register;
      Window_Sec.Register;
      Output_Stage.Register;
      Set_State.Register;
      Output_Format.Register;
      Get_Args.Register;

      -- Initialize algorithm with default values
      PanTompkins.Initialize ((Sampling_Frequency => Sample_Rate.Get_Value, 
                                          Amplitude_Treshold_Coef => Amplitude_Coef.Get_Value,
                                          Minimal_Pick_Distance_Sec => Pick_Distance.Get_Value, 
                                          Window_Sec => Window_Sec.Get_Value, 
                                          Output_Stage => Output_Stage.Get_Value));
   end Initialize;

   procedure Write_Float32 is new UART_USB.Write (T => IEEE_Float_32);
   
   procedure Update_Blocking is
   begin
      begin
         loop
            Read_UART; -- Not Blocking

            case Current_State is
               when SAMPLING =>

                  declare
                     Result : IEEE_Float_32 := 0.0;
                     Status : UART_Status;
                  begin
                     Result := PanTompkins.Process_Sample (AdaData.Data (Sample_Index));
                     Sample_Index := (Sample_Index mod AdaData.Data_Size) + 1;
                     
                     if Output_Format.Get_Value = OUT_ASCII then
                        Log (Sample_Index'Image & "," & Result'Image & CR_LF & CMD_END);
                     elsif Output_Format.Get_Value = IEEE_FLOAT_32_BIGE then
                        Write_Float32 (32.56, UART_USB.BIG_ENDIAN, Status);
                        Log (CMD_END);
                     end if;

                  end;

               when others =>
                  null;
            end case;
         end loop;
      exception -- Unknows Errors (if UART is working..), restart the board
         when E : Constraint_Error =>
            Log (Exception_Message (E) & CMD_END);
         when E : Program_Error => 
            Log (Exception_Message (E) & CMD_END);
      end;
   end Update_Blocking;

end Ecg_Sensor;