with Ecg_Sensor;
with Peripherals; use Peripherals;
with UART_USB; use UART_USB;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
 
with HAL; use HAL;
with HAL.UART; use HAL.UART;
with System; use System;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Sensor_Handler is

   CR_LF : String := ASCII.CR & ASCII.LF;

   package UART_STR renames UART_USB.B_Str;
   package Cmd_Str renames Commands_Interpreter.Command_String;

   Current_Sensor : access Sensor_Type'Class := null;

   procedure Log (This : in out UART_USB.Controller; Msg : String) renames UART_USB.Transmit_String;

   procedure Send_Command (Msg : String) is
   begin
      Log (USBCOM, "<" & Msg & ">");   
   end Send_Command;

   procedure Send_Version (User_Input : Commands_Interpreter.Argument ; Valid : Boolean) is
   begin
      Send_Command (Current_Sensor.Get_Name & " v" & Current_Sensor.Get_Version);
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
         Virtual_ADC.Set_Sample_Rate (Sample_Rate.Get_Value);
         Virtual_ADC.Start_Sampling (Input_Channel.Get_Value);
         Current_Sensor.Start;
      end if;
   end Start_Sampling;

   procedure Stop_Sampling (Input : Commands_Interpreter.Argument; Valid : Boolean) is
   begin
      if Virtual_ADC.Is_Sampling then
         Current_Sensor.Stop;
         Virtual_ADC.Stop_Sampling;
         Virtual_ADC.Reset_Buffer;
      end if;
   end Stop_Sampling;

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

   procedure Send_Sample (Input: Sample; Format : Output_Format_Type) is
   Time_Stamp : UInt32 := UInt32 (To_Duration ((Input.Timestamp) * 1_000)); -- Time Stamp in millisecond
   Status : UART_Status;
   
   procedure Write_UInt_32 is new UART_USB.Write (T => UInt32);
   procedure Write_Float_32 is new UART_USB.Write (T => IEEE_Float_32);

   begin
      case Format is
         when OUT_ASCII =>
            Send_Command (Time_Stamp'Image & ";" & Input.Value'Image & ";" & Current_Sensor.Is_Triggered'Image);
            --Send_Command (Time_Stamp'Image & ";" & Input.Value'Image);
         when FLOAT32 =>
            Write_UInt_32 (USBCOM, Time_Stamp, BIG_ENDIAN, Status);
            Write_Float_32 (USBCOM, Input.Value, BIG_ENDIAN, Status);
            USBCOM.Put_Blocking ((if Current_Sensor.Is_Triggered then 1 else 0), Status, Time_Span_Last);
         when others =>
            null;
      end case;
   end Send_Sample;

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

   procedure Initialize (Sensor : in out Sensor_Type'Class) is
   begin
      Current_Sensor := Sensor'Unchecked_Access;

      USBCOM.Initialize (UART_BAUDRATE);
      USBCOM.Enable_Interrupt;

      -- Parameters
      Output_Format.Register;
      Input_Channel.Register;
      Input_Gain.Register;
      Sample_Rate.Register;

      -- Action
      Get_Args.Register;
      Reset_Cmd.Register;
      Stop_Cmd.Register;
      Start_Cmd.Register;
      Next_Cmd.Register;      
      Version_Cmd.Register;

      Virtual_ADC.Initialize;

      Current_Sensor.Initialize;
   end Initialize;

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

   -- Rename the procedure
   procedure Process_Sample is 
   Next_Sample, Result : Sample;
   begin

      if not Virtual_ADC.Is_Sampling or not Virtual_ADC.Has_Sample then
          return;
      end if;

      Next_Sample := Virtual_ADC.Pop_Sample;
      
      if not Current_Sensor.Process_Sample (Next_Sample, Result) then
         return;
      end if;

      if Next_Cmd.Accessor.Get_Value > 0 then
         Next_Cmd.Accessor.Set_Value (Next_Cmd.Accessor.Get_Value - 1);
         if Next_Cmd.Accessor.Get_Value = 0 then
            Virtual_ADC.Stop_Sampling;
         end if;
      end if;

      if Enable_Trigger.Get_Value and not Current_Sensor.Is_Triggered then
         return;
      end if;

      Send_Sample (Result, Output_Format.Get_Value);

   end Process_Sample;

   procedure Start_Sensor is 
   begin
      loop
         Read_Command; -- Not Blocking
         Process_Sample;
      end loop;
   end Start_Sensor;

end Sensor_Handler;