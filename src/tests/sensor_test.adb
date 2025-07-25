with Commands_Interpreter;
with Virtual_ADC; use Virtual_ADC;
with Ecg_Sensor; use Ecg_Sensor;
with Hook_Test;
with Virtual_Sensor;
with Peripherals; use Peripherals;
with UART_USB;
with Ada.Exceptions; use Ada.Exceptions; 

procedure Sensor_Test is
   Sensor : Ecg_Sensor_Type;
   Sample_Out : Virtual_ADC.Sample;
begin

   USBCOM.Initialize (115_200);

   begin
      Virtual_ADC.Initialize;
      Virtual_ADC.Set_Sample_Rate (1000);
      Virtual_ADC.Start_Sampling (CH_BTN);
     
      Hook_Test.Initialize;

      Sensor.Initialize;
      Sensor.Set_Hook (Hook_Test.Hook'Access, Virtual_Sensor.ON_TRIGGER);
      Sensor.Start;

      loop
         if Virtual_ADC.Has_Sample then
            Sensor.Handle_Sample (Virtual_ADC.Pop_Sample, Sample_Out);
            UART_USB.Transmit_String (USBCOM, Sample_Out.Value'Image & ASCII.CR & ASCII.LF);
         end if;
      end loop;

   exception
      when E : Constraint_Error =>
         UART_USB.Transmit_String (USBCOM, Exception_Message (E));
      when E : Program_Error    => 
         UART_USB.Transmit_String (USBCOM, Exception_Message (E));
      when E : Commands_Interpreter.Commands_Exception=>
         UART_USB.Transmit_String (USBCOM, Exception_Message (E));
   end;

end Sensor_Test;