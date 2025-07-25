with Peripherals; use Peripherals;

package body Hook_Test is

   procedure Initialize is
   begin
      LED_Ctrl.Initialize;
      LED_Ctrl.Set_Frequency (15.0);
   end Initialize;

   procedure Hook (This : in out Virtual_Sensor.Sensor_Type'Class ; S : Virtual_ADC.Sample) is 
   begin
      LED_Ctrl.Start_Blinking;
   end Hook;

end Hook_Test;