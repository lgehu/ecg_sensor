with Ada.Exceptions; use Ada.Exceptions; 

with Sensor_Handler; use Sensor_Handler;
with Ecg_Sensor; 
with Commands_Interpreter;
with Speech_Detector;
with Virtual_Sensor;

with Hook_Test;

procedure Main is
   --Sensor : Ecg_Sensor.Ecg_Sensor_Type;
   Sensor : Speech_Detector.Speech_Detector_Type;

begin

   Hook_Test.Initialize;
   Sensor.Set_Hook (Hook_Test.Hook'Access, Virtual_Sensor.ON_TRIGGER);

   begin
      Sensor_Handler.Initialize (Sensor);
      Sensor_Handler.Start_Sensor;
   exception
      when E : Constraint_Error =>
         Sensor_Handler.Send_Command (Exception_Message (E));
      when E : Program_Error    => 
         Sensor_Handler.Send_Command (Exception_Message (E));
      when E: Commands_Interpreter.Commands_Exception =>
         Sensor_Handler.Send_Command (Exception_Message (E));
   end;
end Main;