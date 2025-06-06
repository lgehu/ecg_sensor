with Commands_Interpreter;
package Ecg_Sensor is

   procedure Initialize;

   procedure Update_Blocking;

   private
      procedure Change_State (Input : Commands_Interpreter.Argument; Valid : Boolean);

end Ecg_Sensor;