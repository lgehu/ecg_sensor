with Ecg_Sensor;
with System;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Bounded;

with HAL; use HAL;
with HAL.UART; use HAL.UART;
with STM32.Board; use STM32.Board;
with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;


with Peripherals; use Peripherals;
with PanTompkins;
with Ada.Unchecked_Conversion;
with UART_USB; use UART_USB;
with ADC_Controller;

package body Ecg_Sensor is

   -- TODO: Add parameter for input channel and output channel selection
   -- TODO: Add Unregister procedure 
   -- TODO: Add this crate to the private alire index
   -- TODO: Add input and output channel (ADC, SPI ...)
   -- TODO: Add the dataset name at the beginning of the data signal ?
   -- TODO: Optimize function Process Sample. It take up to 0.0049 seconds with -02.
   -- Thus, the sensor is limited to 200 Hz if sampling in real time (other than sampling the flash). 
   -- For example, sampling the ADC at 1000 Hz will skip ~5 sample, picks will be detected with smaller interval and calcul higher heart rate.
   -- The problem might come from the UART.

   type Sampling_Mode is (Mode_Loop, Mode_Onetime);
   type Sensor_State_Type is (IDLE, RUNNING, PAUSED);

   package UART_STR renames UART_USB.B_Str;
   package Cmd_Str renames Commands_Interpreter.Command_String;

   ECG_VERSION : constant String := "0.1";
   CR_LF : constant String := ASCII.CR & ASCII.LF;
   CMD_END : constant Character := ASCII.Semicolon;
   Epoch : constant Time := Clock;              -- Start time for sending timestamp       

   Sample_Index : Positive := 1;                -- Current index in the sample data 
   Current_State : Sensor_State_Type := IDLE;   -- Current state of the sensor
   Process_Start_Time : Time := Clock;          -- Precedent sample sent time

   Last_Btn_State : Boolean;

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
   Input : IEEE_Float_32;
   begin
      case Input_Channel.Get_Value is
         when CH_FLASH =>
            Input := AdaData.Data (Sample_Index);
            Sample_Index := (Sample_Index mod AdaData.Data_Size) + 1; 
         when CH_BTN =>
            if not Set (Peripherals.User_Btn) and Last_Btn_State then
               Input := 5000.0;
            else
               Input := 0.0;
            end if;
            Last_Btn_State := Set (Peripherals.User_Btn);
         when CH_ADC =>
               Input := IEEE_Float_32 (ADC_Controller.Read_Value_Blocking);
      end case;
      return PanTompkins.Process_Sample (Input);
   end Next_Value;

   procedure Send_Next_Value (User_Input : Commands_Interpreter.Argument; Valid : Boolean) is
   Result : IEEE_Float_32 := Next_Value;
   Time_Stamp : UInt64 := UInt64 (To_Duration ((Clock - Epoch) * 1_000_000)); -- Time Stamp in microsecond
   Status : UART_Status;
   procedure Write_UInt64 is new UART_USB.Write (T => UInt64);
   begin

      if Enable_Trigger.Get_Value and not PanTompkins.Is_Pick_Detected then
         return;
      end if;

      case Output_Format.Get_Value is
         when OUT_ASCII =>
            Send_Command (Time_Stamp'Image & ";" & Result'Image & ";" & PanTompkins.Is_Pick_Detected'Image);
         when FLOAT32 =>
            Write_UInt64 (USBCOM, Time_Stamp, BIG_ENDIAN, Status);
            Log (USBCOM, ";");
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
               PanTompkins.Initialize ((Sampling_Frequency => PanTompkins.Sampling_Frequency_Type (Sample_Rate.Get_Value), 
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

   procedure Process_Sample is 
   Result : IEEE_Float_32 := 0.0;
   Status : UART_Status;
   Sample_Period : Time_Span := To_Time_Span(1.0 / Sample_Rate.Get_Value);
   Elapsed_Time : Time_Span := Clock - Process_Start_Time;
   begin
      if Elapsed_Time > Sample_Period then
         Process_Start_Time := Clock;

         Send_Next_Value ((others => Cmd_Str.Null_Bounded_String), True);
         -- Log (USBCOM, "Elapsed Time: " &  To_Duration (Elapsed_Time)'Image);
         if PanTompkins.Is_Pick_Detected then
            LED_Ctrl.Start_Blinking;
         end if;         
         -- LED_Ctrl.Set_Frequency (Float (PanTompkins.Get_Heart_Rate / 60.0));
      end if;
   end Process_Sample;

   procedure Initialize is
   begin
      USBCOM.Enable_Interrupt;

      -- Controllers
      LED_Ctrl.Initialize;
      LED_Ctrl.Set_Frequency (15.0);

      Enable_Clock (Peripherals.User_Btn);
      Configure_IO (Peripherals.User_Btn, (Mode_In, Resistors => Pull_Down));

      ADC_Controller.Initialize;

      -- Parameters
      Amplitude_Coef.Register;
      Sample_Rate.Register;
      Pick_Distance.Register;
      Window_Sec.Register;
      Output_Stage.Register;
      Output_Format.Register;
      Input_Channel.Register;
      Enable_Trigger.Register;

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


   procedure Update_Blocking is
   begin
      begin
         loop
            Read_Command; -- Not Blocking

            case Current_State is
               when RUNNING =>
                  Process_Sample;
               when others =>
                  
                  if Next_Cmd.Get_Value > 0 then
                     Process_Sample;
                     Next_Cmd.Accessor.Set_Value (Next_Cmd.Get_Value - 1);
                  end if;

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