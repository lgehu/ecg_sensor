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

   ECG_VERSION : constant String := "0.1";
   LF_CR : constant String := ASCII.LF & ASCII.CR;

   type Sampling_Mode is (Mode_Loop, Mode_Onetime);
   type Sensor_State_Type is (INIT, SAMPLING, PAUSED);

   package UART_STR renames UART_USB.B_Str;

   Sample_Index : Positive := 1;                -- Current index in the sample data 
   Raw_Input : UART_USB.UART_String;            -- Buffer to store incoming commands
   Current_State : Sensor_State_Type := INIT;   -- Current state of the sensor
   
   procedure Log (Msg : String) renames UART_USB.Transmit_String;

   procedure Return_Arg (Arg : Commands_Interpreter.Argument; Valid : Boolean) is
   begin
      if Valid then
         Log (Arg.Key'Image & " = " & Arg.Value'Image & LF_CR);
      else
         Log ("Invalid parameter" & LF_CR);
      end if;
   end;

   procedure Print_Args (Input : Commands_Interpreter.Argument; Valid : Boolean) is
   Args : Commands_Interpreter.Arg_Array (1 .. Commands_Interpreter.Get_Arg_Count);
   begin
      Commands_Interpreter.Get_Args (Args);
      for I in Args'Range loop
         Log (Args (I).Key'Image & " = " & Args (I).Value'Image & LF_CR);
      end loop;
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

   procedure Read_UART is
   Status : UART_Status;
   Char : Character;
   Arg : Commands_Interpreter.Argument;
   Data : UInt9;
   begin
      UART_USB.Read_Blocking (Data, Status, Milliseconds (0));
      if Status = HAL.UART.Ok then
         Char := Character'Val (Data);
         if UART_STR.Length (Raw_Input) >= UART_STR.Max_Length - 10 or Char = ASCII.Semicolon then
            Arg := Commands_Interpreter.Parse (UART_STR.To_String (Raw_Input));
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
               if Current_State = SAMPLING or Current_State = PAUSED then
                  Current_State := INIT;
               end if;
            when PAUSED =>
               if Current_State = SAMPLING then
                  Current_State := PAUSED;
               end if;
         end case;

         if Current_State = Old_State then
            Log ("Can't change state from " & 
                  Current_State'Image & " to " & Intended_State'Image & LF_CR);
            return False;
         end if;

         return True;

      exception
         when E : Constraint_Error =>
            Log (Exception_Message (E));
            return False;
      end;

      return True;

   end Valide_State;

   procedure Initialize is
   begin
      UART_USB.Initialize (115_200);
      UART_USB.Transmit_String ("ECG_SENSOR v0.1" & LF_CR);

      Amplitude_Coef.Register;
      Sample_Rate.Register;
      Pick_Distance.Register;
      Window_Sec.Register;
      Output_Stage.Register;
      Set_State.Register;
      Get_Args.Register;

      -- Initialize algorithm with default values
      PanTompkins.Initialize ((Sampling_Frequency => Sample_Rate.Get_Value, 
                                          Amplitude_Treshold_Coef => Amplitude_Coef.Get_Value,
                                          Minimal_Pick_Distance_Sec => Pick_Distance.Get_Value, 
                                          Window_Sec => Window_Sec.Get_Value, 
                                          Output_Stage => Output_Stage.Get_Value));
   end Initialize;
   
   procedure Update_Blocking is
   begin
      loop
         begin
            Read_UART; -- Not Blocking

            case Current_State is
               when SAMPLING =>

                  declare
                     Result : IEEE_Float_32 := 0.0;
                  begin
                     Result := PanTompkins.Process_Sample (AdaData.Data (Sample_Index));
                     Sample_Index := (Sample_Index mod AdaData.Data_Size) + 1;
                     Log (Sample_Index'Image & ";" & Result'Image & LF_CR);
                  end;

               when others =>
                  null;
            end case;

         exception -- Unknows Errors (if UART is working..)
            when E : Constraint_Error =>
               Log (Exception_Message (E));
            when E : Program_Error => 
               Log (Exception_Message (E));
            when E : Commands_Interpreter.Commands_Exception =>
               Log (Exception_Message (E));
         end;
      end loop;
   end Update_Blocking;

end Ecg_Sensor;
