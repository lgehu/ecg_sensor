with Ecg_Sensor;
with Peripherals; use Peripherals;
with Ada.Exceptions; use Ada.Exceptions; 
with UART_USB; use UART_USB;

procedure Main is
begin
   USBCOM.Initialize (115_200);
   begin
      Ecg_Sensor.Initialize;
      Ecg_Sensor.Update_Blocking;
   exception
      when E : Constraint_Error =>
         Transmit_String (USBCOM, Exception_Message (E));
   end;
end Main;