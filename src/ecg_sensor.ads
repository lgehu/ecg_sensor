with Commands_Interpreter;
package Ecg_Sensor is

   procedure Initialize;

   procedure Update_Blocking;

   private
      function Valide_State (Input : Commands_Interpreter.Argument) return Boolean;

end Ecg_Sensor;