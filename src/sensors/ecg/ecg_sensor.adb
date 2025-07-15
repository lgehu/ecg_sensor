with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Bounded;
with Ada.Unchecked_Conversion;

with HAL;           use HAL;
with HAL.UART;      use HAL.UART;
with STM32.Board;   use STM32.Board;
with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;

with Peripherals;   use Peripherals;
with UART_USB;      use UART_USB;

with Virtual_ADC;

with Interfaces; use Interfaces;

package body Ecg_Sensor is

   -- TODO: Add parameter for input channel and output channel selection
   -- TODO: Add Unregister procedure 
   -- TODO: Add this crate to the private alire index
   -- TODO: Add input and output channel (ADC, SPI ...)
   -- TODO: Add the dataset name at the beginning of the data signal ?
   -- TODO: Add error check in UART interrupt

   SENSOR_NAME    : constant String := "ECG Sensor";
   SENSOR_VERSION : constant String := "1.0";

   
   overriding 
   function Get_Version (This : in out Ecg_Sensor_Type) return String is
   begin
      return SENSOR_VERSION;
   end Get_Version;

   overriding function Get_Name (This : in out Ecg_Sensor_Type) return String is
   begin
      return SENSOR_NAME;
   end Get_Name;

   overriding
   procedure Start (This : in out Ecg_Sensor_Type) is
   begin
      PanTompkins.Initialize ((Sampling_Frequency => PanTompkins.Sampling_Frequency_Type (Sample_Rate.Get_Value), 
                              Amplitude_Treshold_Coef => Amplitude_Coef.Get_Value,
                              Minimal_Peak_Distance_Sec => Peak_Distance.Get_Value, 
                              Window_Sec => Window_Sec.Get_Value, 
                              Output_Stage => Output_Stage.Get_Value));
   end Start;

   overriding
   procedure Stop (This : in out Ecg_Sensor_Type) is
   begin
      null;
   end Stop;

   overriding 
   function  Process_Sample 
   (This : in out Ecg_Sensor_Type; 
   Sample_In : Virtual_ADC.Sample ; 
   Sample_Out : out Virtual_ADC.Sample)
   return Boolean is
   Result : IEEE_Float_32 := 0.0;
   begin
      Result := PanTompkins.Process_Sample (Sample_In.Value);

      if PanTompkins.Is_Peak_Detected then
         LED_Ctrl.Start_Blinking;
      end if;

      -- Send sample if there is a peak and trigger is enabled 
      if (Enable_Trigger.Get_Value and not PanTompkins.Is_Peak_Detected) then
         return False;
      end if;

      return True;
   end Process_Sample;

   overriding
   procedure Initialize (This : in out Ecg_Sensor_Type) is
   begin
      -- Controllers
      LED_Ctrl.Initialize;
      LED_Ctrl.Set_Frequency (15.0);

      -- Parameters
      Amplitude_Coef.Register;
      Peak_Distance.Register;
      Enable_Trigger.Register;
      Window_Sec.Register;
      Output_Stage.Register;

      PanTompkins.Initialize ((Sampling_Frequency => PanTompkins.Sampling_Frequency_Type (Sample_Rate.Get_Value), 
                           Amplitude_Treshold_Coef => Amplitude_Coef.Get_Value,
                           Minimal_Peak_Distance_Sec => Peak_Distance.Get_Value, 
                           Window_Sec => Window_Sec.Get_Value, 
                           Output_Stage => Output_Stage.Get_Value));
   end Initialize;

end Ecg_Sensor;