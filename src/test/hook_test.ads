with Virtual_Sensor;
with Virtual_ADC;

package Hook_Test is

   procedure Initialize;

   procedure Hook (This : in out Virtual_Sensor.Sensor_Type'Class ; S : Virtual_ADC.Sample);

end Hook_Test;