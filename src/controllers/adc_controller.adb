with STM32.Device;   use STM32.Device; use STM32.Device;
with STM32.Board; use STM32.Board;
with STM32.ADC; use STM32.ADC;
with STM32.GPIO; use STM32.GPIO;
with HAL; use HAL;

with Peripherals; use Peripherals;

package body ADC_Controller is 

   All_Regular_Conversions : constant Regular_Channel_Conversions :=
          (1 => (Channel => ADC_Input_Channel, Sample_Time => Sample_144_Cycles)); 

   procedure Initialize is 
   begin
      Enable_Clock (ADC_Input);
      Configure_IO (ADC_Input, (Mode => Mode_Analog, Resistors => Floating));

      Enable_Clock(ADC_Converter);

      Configure_Common_Properties
      (Mode           => Independent,
         Prescalar      => PCLK2_Div_2,
         DMA_Mode       => Disabled,
         Sampling_Delay => Sampling_Delay_5_Cycles);

      Configure_Unit
      (ADC_Converter,
         Resolution => ADC_Resolution_12_Bits,
         Alignment  => Right_Aligned);

      Configure_Regular_Conversions
      (ADC_Converter,
         Continuous  => False,
         Trigger     => Software_Triggered,
         Enable_EOC  => True,
         Conversions => All_Regular_Conversions);

      Enable (ADC_Converter);
   end Initialize;

   function Read_Value_Blocking return Unsigned_16 is
   Successful : Boolean := False;
   begin
      Start_Conversion (ADC_Converter);
      Poll_For_Status (ADC_Converter, Regular_Channel_Conversion_Complete, Successful);
      if Successful then
         return Unsigned_16 (Conversion_Value (ADC_Converter));
      end if;
      return 0;
   end Read_Value_Blocking;

end ADC_Controller;