with Ecg_Sensor;
with System;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Bounded;

with HAL; use HAL;
with HAL.UART; use HAL.UART;

with Peripherals; use Peripherals;
with PanTompkins;
with AdaData;
with Ada.Unchecked_Conversion;
with UART_USB; use UART_USB;

package body Ecg_Sensor is

   -- TODO: Add parameter for input channel and output channel selection
   -- TODO: Command to acquire an arbitrary ammount of sample (ACQUIRE=100)
   -- TODO: Documentation for commands and interpreter
   -- TODO: Add Unregister procedure 
   -- TODO: Add this crate to the private alire index
   -- TODO: Add input and output channel (ADC, SPI ...)
   
   ECG_VERSION : constant String := "0.1";
   CR_LF : constant String := ASCII.CR & ASCII.LF;

   CMD_END : constant Character := ASCII.Semicolon;

   type Sampling_Mode is (Mode_Loop, Mode_Onetime);
   type Sensor_State_Type is (IDLE, RUNNING, PAUSED);

   package UART_STR renames UART_USB.B_Str;
   package Cmd_Str renames Commands_Interpreter.Command_String;

   Sample_Index : Positive := 1;                -- Current index in the sample data 
   Raw_Input : UART_USB.UART_String;            -- Buffer to store incoming commands
   Current_State : Sensor_State_Type := IDLE;   -- Current state of the sensor
   Process_Start_Time : Time := Clock;          -- Precedent sample sent time
   
   procedure Log (This : in out Controller; Msg : String) renames UART_USB.Transmit_String;

   procedure Send_Command (Msg : String) is
   begin
      Log (USBCOM, "<" & Msg & ">");   
   end Send_Command;

   procedure Send_Version (User_Input : Commands_Interpreter.Argument ; Valid : Boolean) is
   begin
      Send_Command ("ECG_SENSOR v" & ECG_VERSION);
      LED_Ctrl.Start_Blinking;
   end Send_Version;

   procedure Return_Arg (User_Input : Commands_Interpreter.Argument; Valid : Boolean) is
   begin
      if Valid then
         Send_Command ("OK");
      else
         Send_Command ("Invalid parameter");
      end if;
   end;

   procedure Reset_Sensor (User_Input : Commands_Interpreter.Argument; Valid : Boolean) is
   SCB_AIRCR : Unsigned_32 with Address => System'To_Address (16#E000ED0C#), Volatile;
   begin
      Send_Command ("OK");
      delay 0.1;
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

      Log (USBCOM, "<");
      if Cmd_Str.Length (User_Input.Value) > 0 then
         Log (USBCOM, Commands_Interpreter.Get_Value (Cmd_Str.To_String (User_Input.Value)));
      else
         for I in Args'Range loop
            -- Print key=value
            Log (USBCOM, Cmd_Str.To_String (Args (I).Key) & "=" & 
                  Args (I).To_String.all & CR_LF);
         end loop;
      end if;
      Log (USBCOM, ">");
   end Print_Args;

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
            USBCOM.Put_Blocking (Escape_Byte, Status);
            USBCOM.Put_Blocking (Escape_Byte + 1, Status);
         elsif Byte9 = Escape_Byte then
            USBCOM.Put_Blocking (Escape_Byte, Status);
            USBCOM.Put_Blocking (Escape_Byte + 2, status);
         else
            USBCOM.Put_Blocking (Byte9, Status);
         end if;
      end loop;
   end Transmit_Float_32;

   function Next_Value return IEEE_Float_32 is
   Result : IEEE_Float_32 := 0.0;
   begin
      Result := PanTompkins.Process_Sample (AdaData.Data (Sample_Index));
      Sample_Index := (Sample_Index mod AdaData.Data_Size) + 1; 
      return Result;
   end Next_Value;

   procedure Send_Next_Value (User_Input : Commands_Interpreter.Argument; Valid : Boolean) is
   Result : IEEE_Float_32 := Next_Value;
   begin
      case Output_Format.Get_Value is
         when OUT_ASCII =>
            Send_Command (Result'Image);
         when FLOAT32 =>
            Transmit_Float_32 (Result);
            Log (USBCOM, CMD_END & "");
         when others =>
            null;
      end case;
   end Send_Next_Value;

   procedure Read_Command is
   Arg : Commands_Interpreter.Argument;
   begin
      if USBCOM.Has_Data then
         begin
            Arg := Commands_Interpreter.Parse (UART_STR.To_String (USBCOM.Get_Data), '=');
         exception
            when E : Commands_Interpreter.Commands_Exception =>
               Send_Command (Exception_Message (E));
         end;
         USBCOM.Enable_Interrupt;
      end if;
   end Read_Command;

   procedure Change_State (Input : Commands_Interpreter.Argument; Valid : Boolean) is
   Intended_State : Sensor_State_Type;
   Old_State : Sensor_State_Type := Current_State;
   Cmd_Key : String := Cmd_Str.To_String (Input.Key);
   begin
      begin
         if Cmd_Key = "START" then
            Intended_State := RUNNING;
            if Current_State = IDLE then
               Current_State := RUNNING;
               
               Sample_Index := 1;
               PanTompkins.Initialize ((Sampling_Frequency => PanTompkins.Sampling_Frequency_Type (AdaData.Sample_Rate), 
                                       Amplitude_Treshold_Coef => Amplitude_Coef.Get_Value,
                                       Minimal_Pick_Distance_Sec => Pick_Distance.Get_Value, 
                                       Window_Sec => Window_Sec.Get_Value, 
                                       Output_Stage => Output_Stage.Get_Value));


            elsif Current_State = PAUSED then
               Current_State := RUNNING;
            end if;
         elsif Cmd_Key = "STOP" then

            Intended_State := IDLE;
            if Current_State = RUNNING or 
                  Current_State = PAUSED or 
                  Current_State = IDLE then
               Current_State := IDLE;
            end if;
         elsif Cmd_Key = "PAUSE" then
            Intended_State := PAUSED; 
            if Current_State = RUNNING then
               Current_State := PAUSED;
            end if;
         else
            Send_Command ("Invalid action");
         end if;

         if Intended_State /= Current_State then
            Send_Command ("Can't change state from " & 
                  Current_State'Image & " to " & Intended_State'Image);
         else
            Send_Command ("OK");
         end if;

      exception
         when E : Constraint_Error =>
            Send_Command (Exception_Message (E));
      end;
   end Change_State;

   procedure Initialize is
   begin
      USBCOM.Enable_Interrupt;

      -- Controllers
      LED_Ctrl.Initialize;
      LED_Ctrl.Set_Frequency (10.0);

      -- Parameters
      Amplitude_Coef.Register;
      Sample_Rate.Register;
      Pick_Distance.Register;
      Window_Sec.Register;
      Output_Stage.Register;
      Output_Format.Register;

      -- Action
      Get_Args.Register;
      Reset_Cmd.Register;
      Stop_Cmd.Register;
      Start_Cmd.Register;
      Pause_Cmd.Register;
      Next_Cmd.Register;      
      Version_Cmd.Register;

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
         Send_Next_Value ((others => Cmd_Str.Null_Bounded_String), True);
         
         if PanTompkins.Is_Pick_Detected then
            LED_Ctrl.Start_Blinking;
         end if;         
         -- LED_Ctrl.Set_Frequency (Float (PanTompkins.Get_Heart_Rate / 60.0));
      end if;
   end Process_Sample;

   procedure Update_Blocking is
   begin
      begin
         loop
            Read_Command; -- Not Blocking

            case Current_State is
               when RUNNING =>
                  Process_Sample;
               when others =>
                  null;
            end case;
         end loop;
      exception -- Unknows Errors (if UART is working..), restart the board
         when E : Constraint_Error =>
            Send_Command (Exception_Message (E));
         when E : Program_Error => 
            Send_Command (Exception_Message (E));
      end;
   end Update_Blocking;

end Ecg_Sensor;