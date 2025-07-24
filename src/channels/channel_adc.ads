with Generic_Channel;

with HAL;
with HAL.GPIO;

with STM32.ADC; use STM32.ADC;
with STM32.GPIO; use STM32.GPIO;
with Channels;

package Channel_ADC is
   -- TODO: On the F446RE, others GPIO than PA0 does not work
   package Channel_32bits is new Generic_Channel (Value_Type => HAL.UInt32);

   type Channel_ADC_Type (Buffer_Size : Natural ; 
                        ADC_GPIO : access GPIO_Point ; 
                        ADC_Channel : Analog_Input_Channel;
                        ADC_Converter : access STM32.ADC.Analog_To_Digital_Converter)
   is new Channel_32bits.Generic_Channel_Type (Buffer_Size) and Channels.Channel with null record;

   overriding
   procedure Open_Channel (This : in out Channel_ADC_Type);

   overriding
   procedure Close_Channel (This : in out Channel_ADC_Type);

   overriding
   procedure Read_Channel (This : in out Channel_ADC_Type);

   overriding
   function Is_Open (This : in out Channel_ADC_Type) return Boolean;

end Channel_ADC;