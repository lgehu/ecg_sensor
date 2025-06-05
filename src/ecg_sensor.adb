with System;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Bounded;
with Interfaces; use Interfaces;

with HAL; use HAL;
with HAL.UART; use HAL.UART;

with UART_USB;
with PanTompkins;
with AdaData;
with Ada.Unchecked_Conversion;

package body Ecg_Sensor is

   -- TODO: Add parameter for input channel and output channel selection
   -- TODO: Command to acquire an arbitrary ammount of sample (ACQUIRE=100)
   -- TODO: Resolve bug of overflow from the UART 
   -- TODO: Documentation for commands and interpreter

   ECG_VERSION : constant String := "0.1";
   CR_LF : constant String := ASCII.CR & ASCII.LF;

   CMD_END : constant Character := ASCII.Semicolon;

   type Sampling_Mode is (Mode_Loop, Mode_Onetime);
   type Sensor_State_Type is (INIT, SAMPLING, PAUSED);

   type Output_Format_Type is (OUT_ASCII, FLOAT32);

   type Command_Read_State is (WAITING, PARSING);

   package UART_STR renames UART_USB.B_Str;
   package Cmd_Str renames Commands_Interpreter.Command_String;

   Sample_Index : Positive := 1;                -- Current index in the sample data 
   Raw_Input : UART_USB.UART_String;            -- Buffer to store incoming commands
   Current_State : Sensor_State_Type := INIT;   -- Current state of the sensor
   Process_Start_Time : Time := Clock;          -- Precedent sample sent time
   Last_Char_Time : Time := Clock;              -- Last received char from UART
   Command_State : Command_Read_State := WAITING;
   
   procedure Log (Msg : String) renames UART_USB.Transmit_String;

   procedure Return_Arg (User_Input : Commands_Interpreter.Argument; Valid : Boolean) is
   begin
      if Valid then
         Log ("OK" & CMD_END);
      else
         Log ("Invalid parameter" & CMD_END);
      end if;
   end;

   procedure Reset_Sensor (User_Input : Commands_Interpreter.Argument; Valid : Boolean) is
      SCB_AIRCR : Unsigned_32 with Address => System'To_Address (16#E000ED0C#),
                                 Volatile;
   begin
      Log ("OK" & CMD_END);
      SCB_AIRCR := 16#05FA0004#;
      loop
         null; -- Wait reset
      end loop;
   end Reset_Sensor;

   procedure Print_Args (User_Input : Commands_Interpreter.Argument; Valid : Boolean) is
   Args : Commands_Interpreter.Arg_Array (1 .. Commands_Interpreter.Get_Arg_Count);
   Index : Natural;
   begin
      Commands_Interpreter.Get_Args (Args);

      if Cmd_Str.Length (User_Input.Value) > 0 then
         Log (Cmd_Str.To_String (
               Commands_Interpreter.Find_Arg (
                     Cmd_Str.To_String (User_Input.Value), Index).Value));
      else
         for I in Args'Range loop
            -- Print key=value
            Log (Cmd_Str.To_String (Args (I).Key) & "=" & 
                  Cmd_Str.To_String (Args (I).Value) & CR_LF);
         end loop;
      end if;
      Log(CMD_END & "");
   end Print_Args;

   package Sample_Rate is new Commands_Interpreter.Discrete_Accessor (T => Positive,
               Key            => "SAMPLE_RATE",
               Default_Value  => 100,
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
               Default_Value => FLOAT32,
               Action_Fn => Return_Arg'Access
            );

   package Reset is new Commands_Interpreter.Action_Accessor (
               Key => "RESET", 
               Action_Fn => Reset_Sensor'Access
            );

   procedure Read_Command is
   Status : UART_Status := HAL.UART.Ok;
   Char : Character;
   Arg : Commands_Interpreter.Argument;
   Data : UInt9;
   begin
      while Status = HAL.UART.Ok loop 
         UART_USB.Read_Blocking (Data, Status, Milliseconds (1));
         if Status = HAL.UART.Ok then
            Char := Character'Val (Data);
            case Command_State is
               when WAITING =>
                  if Char = '<' then
                     Raw_Input := UART_STR.Null_Bounded_String;
                     Command_State := PARSING;
                     Last_Char_Time := Clock;
                  end if;
               when PARSING =>
                  if Char = '>' then
                     begin
                        Arg := Commands_Interpreter.Parse (UART_STR.To_String (Raw_Input));
                     exception
                        when E : Commands_Interpreter.Commands_Exception =>
                           Log (Exception_Message (E) & CR_LF & CMD_END);
                     end;
                     Raw_Input := UART_STR.Null_Bounded_String;
                     Command_State := WAITING;
                  else
                     -- Max Buffer length or timeout reached 
                     if (UART_STR.Length (Raw_Input) + 1) >= UART_STR.Max_Length or
                        (Clock - Last_Char_Time) > To_Time_Span(5.0) then
                        Command_State := WAITING;
                        Log ("<ERROR_PARSING>");
                     else
                        UART_STR.Append (Raw_Input, Char);
                        Last_Char_Time := Clock;
                     end if;
                  end if;
            end case;
         end if;
      end loop;
   end Read_Command;

   -- Send binary float with escape value
   procedure Transmit_Float_32 (Data : IEEE_Float_32) is
   type Byte_Array is array (1 .. 4) of UInt8;
   function To_Bytes is new Ada.Unchecked_Conversion (IEEE_Float_32, Byte_Array);
   Raw_Bytes : Byte_Array := To_Bytes (Data);
   
   Escape_Byte : constant UInt9 := 16#07D#; 
   Semicolon_Byte : constant UInt9 := 16#03B#;

   Status : UART_Status;
   Byte9 : UInt9;

   begin
      for Byte of reverse Raw_Bytes loop
         Byte9 := UInt9 (Byte);
         if Byte9 = Semicolon_Byte then
            UART_USB.Put_Blocking (Escape_Byte, Status);
            UART_USB.Put_Blocking (Escape_Byte + 1, Status);
         elsif Byte9 = Escape_Byte then
            UART_USB.Put_Blocking (Escape_Byte, Status);
            UART_USB.Put_Blocking (Escape_Byte + 2, status);
         else
            UART_USB.Put_Blocking (Byte9, Status);
         end if;
      end loop;
   end Transmit_Float_32;

   function Valide_State (Input : Commands_Interpreter.Argument) return Boolean is
   Intended_State : Sensor_State_Type;
   Old_State : Sensor_State_Type := Current_State;
   begin

      begin
         Intended_State := Sensor_State_Type'Value (Cmd_Str.To_String (Input.Value));
         
         case Intended_State is
            when SAMPLING =>
               if Current_State = INIT then
                  Current_State := SAMPLING;

                  PanTompkins.Initialize ((Sampling_Frequency => PanTompkins.Sampling_Frequency_Type (AdaData.Sample_Rate), 
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
      UART_USB.Transmit_String ("ECG_SENSOR v0.1" & CR_LF & CMD_END);

      Amplitude_Coef.Register;
      Sample_Rate.Register;
      Pick_Distance.Register;
      Window_Sec.Register;
      Output_Stage.Register;
      Set_State.Register;
      Output_Format.Register;
      Get_Args.Register;
      Reset.Register;

      -- Initialize algorithm with default values
      PanTompkins.Initialize ((Sampling_Frequency => PanTompkins.Sampling_Frequency_Type (AdaData.Sample_Rate), 
                                          Amplitude_Treshold_Coef => Amplitude_Coef.Get_Value,
                                          Minimal_Pick_Distance_Sec => Pick_Distance.Get_Value, 
                                          Window_Sec => Window_Sec.Get_Value, 
                                          Output_Stage => Output_Stage.Get_Value));
   end Initialize;

   procedure Process_Sample is 
   Result : IEEE_Float_32 := 0.0;
   Status : UART_Status;
   Sample_Period : Time_Span := To_Time_Span(1.0 / Sample_Rate.Get_Value);
   begin
      if (Clock - Process_Start_Time) > Sample_Period then
         Process_Start_Time := Clock;

         Result := PanTompkins.Process_Sample (AdaData.Data (Sample_Index));
         Sample_Index := (Sample_Index mod AdaData.Data_Size) + 1;
         
         if Output_Format.Get_Value = OUT_ASCII then
            Log (Sample_Index'Image & "," & Result'Image & CR_LF & CMD_END);
         elsif Output_Format.Get_Value = FLOAT32 then
            Transmit_Float_32 (Result);
            Log (CMD_END & "");
         end if;
      end if;
   end Process_Sample;

   procedure Update_Blocking is
   begin
      begin
         loop
            Read_Command; -- Not Blocking

            case Current_State is
               when SAMPLING =>
                  Process_Sample;
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