with Ecg_Sensor;
with System;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Bounded;
with Ada.Unchecked_Conversion;

with HAL;           use HAL;
with HAL.UART;      use HAL.UART;
with STM32.Board;   use STM32.Board;
with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;

with Peripherals;   use Peripherals;
with UART_USB;      use UART_USB;
with PanTompkins;

with Virtual_ADC;   


package body Ecg_Sensor is

   -- TODO: Add parameter for input channel and output channel selection
   -- TODO: Add Unregister procedure 
   -- TODO: Add this crate to the private alire index
   -- TODO: Add input and output channel (ADC, SPI ...)
   -- TODO: Add the dataset name at the beginning of the data signal ?

   -- TODO: Add error check in UART interrupt

   package UART_STR renames UART_USB.B_Str;
   package Cmd_Str renames Commands_Interpreter.Command_String;

   ECG_VERSION : constant String := "0.1";
   CR_LF : constant String := ASCII.CR & ASCII.LF;
   CMD_END : constant Character := ASCII.Semicolon;

   procedure Log (This : in out UART_USB.Controller; Msg : String) renames UART_USB.Transmit_String;

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

   procedure Start_Sampling (Input : Commands_Interpreter.Argument; Valid : Boolean) is
   begin
      if not Virtual_ADC.Is_Sampling then
         Init_Sampling ((others => Cmd_Str.Null_Bounded_String), True);
         Virtual_ADC.Set_Sample_Rate (Sample_Rate.Get_Value);
         Virtual_ADC.Start_Sampling (Input_Channel.Get_Value);
      end if;
   end Start_Sampling;

   procedure Stop_Sampling (Input : Commands_Interpreter.Argument; Valid : Boolean) is
   begin
      if Virtual_ADC.Is_Sampling then
         Virtual_ADC.Stop_Sampling;
      end if;
   end Stop_Sampling;

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

   procedure Send_Sample (Input: Sample; Format : Output_Format_Type) is
   Time_Stamp : UInt32 := UInt32 (To_Duration ((Input.Timestamp) * 1_000)); -- Time Stamp in millisecond
   Status : UART_Status;
   
   procedure Write_UInt_32 is new UART_USB.Write (T => UInt32);
   procedure Write_Float_32 is new UART_USB.Write (T => IEEE_Float_32);

   begin
      case Format is
         when OUT_ASCII =>
            Send_Command (Time_Stamp'Image & ";" & Input.Value'Image & ";" & PanTompkins.Is_Peak_Detected'Image);
         when FLOAT32 =>
            Write_UInt_32 (USBCOM, Time_Stamp, BIG_ENDIAN, Status);
            Write_Float_32 (USBCOM, Input.Value, BIG_ENDIAN, Status);
            USBCOM.Put_Blocking ((if PanTompkins.Is_Peak_Detected then 1 else 0), Status, Time_Span_Last);
         when others =>
            null;
      end case;
   end Send_Sample;

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

   procedure Init_Sampling (User_Input : Commands_Interpreter.Argument ; Valid : Boolean) is
   begin
      PanTompkins.Initialize ((Sampling_Frequency => PanTompkins.Sampling_Frequency_Type (Sample_Rate.Get_Value), 
                              Amplitude_Treshold_Coef => Amplitude_Coef.Get_Value,
                              Minimal_Peak_Distance_Sec => Peak_Distance.Get_Value, 
                              Window_Sec => Window_Sec.Get_Value, 
                              Output_Stage => Output_Stage.Get_Value));
   end Init_Sampling;
 
   procedure Process_Sample is 
   Result : IEEE_Float_32 := 0.0;
   Status : UART_Status;
   Sample_Period : Time_Span := To_Time_Span(1.0 / Sample_Rate.Get_Value);
   Next_Sample : Sample;
   begin

      if not Virtual_ADC.Has_Sample then
          return;
      end if;

      Next_Sample := Virtual_ADC.Pop_Sample;
      Result := PanTompkins.Process_Sample (Next_Sample.Value);

      if Next_Cmd.Accessor.Get_Value > 0 then
         Next_Cmd.Accessor.Set_Value (Next_Cmd.Accessor.Get_Value - 1);
         if Next_Cmd.Accessor.Get_Value = 0 then
            Virtual_ADC.Stop_Sampling;
         end if;
      end if;

      if PanTompkins.Is_Peak_Detected then
         LED_Ctrl.Start_Blinking;
      end if;

      -- Send sample if there is a peak and trigger is enabled 
      if (Enable_Trigger.Get_Value and not PanTompkins.Is_Peak_Detected) then
         return;
      end if;

      Send_Sample ((Value => Result, 
                  Timestamp => Next_Sample.Timestamp,
                  Channel_Source => Next_Sample.Channel_Source), Output_Format.Get_Value);

   end Process_Sample;

   procedure Initialize is
   begin
      USBCOM.Enable_Interrupt;

      Virtual_ADC.Initialize;

      -- Controllers
      LED_Ctrl.Initialize;
      LED_Ctrl.Set_Frequency (15.0);

      -- Parameters
      Amplitude_Coef.Register;
      Sample_Rate.Register;
      Peak_Distance.Register;
      Window_Sec.Register;
      Output_Stage.Register;
      Output_Format.Register;
      Input_Channel.Register;
      Enable_Trigger.Register;
      Input_Gain.Register;

      -- Action
      Get_Args.Register;
      Reset_Cmd.Register;
      Stop_Cmd.Register;
      Start_Cmd.Register;
      Next_Cmd.Register;      
      Version_Cmd.Register;
      Init_Cmd.Register;

      Init_Sampling ((others => Cmd_Str.Null_Bounded_String), True); 
   end Initialize;

   procedure Update_Blocking is
   begin
      loop
         Read_Command; -- Not Blocking
         Process_Sample;
      end loop;
   end Update_Blocking;

end Ecg_Sensor;