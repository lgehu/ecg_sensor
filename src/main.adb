with Ecg_Sensor;
with UART_USB;
with Ada.Exceptions; use Ada.Exceptions; 

procedure Main is
begin
   begin
      Ecg_Sensor.Initialize;
      Ecg_Sensor.Update_Blocking;
   exception
      when E : Constraint_Error =>
         UART_USB.Transmit_String (Exception_Message (E));
   end;
end Main;