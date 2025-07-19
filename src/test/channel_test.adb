with Channel_ADC; use Channel_ADC;
with Peripherals; use Peripherals;
with STM32.Device; use STM32.Device;
with UART_USB;
with Ada.Exceptions; use Ada.Exceptions;

procedure Channel_Test is
   Channel : Channel_ADC_Type (Buffer_Size => 50, 
                              ADC_GPIO => PA0'Access, 
                              ADC_Channel => 0,
                              ADC_Converter => ADC_1'Access);

   S : Channel_ADC.Channel_32bits.Sample;
begin

   USBCOM.Initialize (115_200);
   UART_USB.Transmit_String (USBCOM, "Test");

   begin

      Channel.Open_Channel;
      
      loop
         Channel.Read_Channel;

         if Channel.Has_Sample then
            S := Channel.Pop_Sample;
            UART_USB.Transmit_String (USBCOM, S.Value'Image & ASCII.CR & ASCII.LF);
         end if;
      end loop;

      Channel.Close_Channel;

   exception
      when E : Constraint_Error =>
         UART_USB.Transmit_String (USBCOM, Exception_Message (E));
      when E : Program_Error    => 
         UART_USB.Transmit_String (USBCOM, Exception_Message (E));
   end;

end Channel_Test;