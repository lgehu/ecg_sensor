package body Virtual_Sensor is

   procedure Set_Hook (This : in out Sensor_Type ; Hook : Hook_Type) is
   begin
      This.Hook := Hook;
   end Set_Hook;

end Virtual_Sensor;