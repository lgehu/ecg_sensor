with STM32.Device;  use STM32.Device;
with STM32.ADC;     use STM32.ADC;
with STM32.Board;   use STM32.Board;
with STM32.GPIO;    use STM32.GPIO;

package body Channel_ADC is

   procedure Initialize_ADC (This : in out Channel_ADC_Type) is
    All_Regular_Conversions : constant Regular_Channel_Conversions :=
   (1 => (Channel => This.ADC_Channel, Sample_Time => Sample_15_Cycles));
   begin
      Enable_Clock (This.ADC_GPIO.all);
      Configure_IO (This.ADC_GPIO.all, (Mode => Mode_Analog, Resistors => Floating));

      Enable_Clock(This.ADC_Converter.all);

      Configure_Common_Properties
      (Mode           => Independent,
         Prescalar      => PCLK2_Div_2,
         DMA_Mode       => Disabled,
         Sampling_Delay => Sampling_Delay_5_Cycles);

      Configure_Unit
      (This.ADC_Converter.all,
         Resolution => ADC_Resolution_12_Bits,
         Alignment  => Right_Aligned);

      Configure_Regular_Conversions
      (This.ADC_Converter.all,
         Continuous  => False,
         Trigger     => Software_Triggered,
         Enable_EOC  => True,
         Conversions => All_Regular_Conversions);

      --Enable_Interrupts (ADC_Converter, Regular_Channel_Conversion_Complete);
      Enable (This.ADC_Converter.all);

   end Initialize_ADC;

   overriding 
   procedure Open_Channel (This : in out Channel_ADC_Type) is
   begin
      Initialize_ADC (This);
      This.Open := True;
   end Open_Channel;

   overriding 
   procedure Close_Channel (This : in out Channel_ADC_Type) is
   begin
      Disable (This.ADC_Converter.all);
      This.Open := False;
   end Close_Channel;
   
   overriding
   procedure Read_Channel (This : in out Channel_ADC_Type) is
   Successful : Boolean;
   begin
      Start_Conversion (This.ADC_Converter.all);
      Poll_For_Status (This.ADC_Converter.all, Regular_Channel_Conversion_Complete, Successful);
      if Successful then
         This.Add_Sample (HAL.UInt32 (Conversion_Value (This.ADC_Converter.all)));
      end if;
   end Read_Channel;

   overriding
   function Is_Open (This : in out Channel_ADC_Type) return Boolean is 
   begin
      return This.Open;
   end Is_Open;


end Channel_ADC;