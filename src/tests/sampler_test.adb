with Generic_Channel;
with Channel_ADC;
with Sampler; use Sampler;
with Channel_ADC; use Channel_ADC;
with Peripherals; use Peripherals;
with STM32.Device; use STM32.Device;
with UART_USB;
with Ada.Exceptions; use Ada.Exceptions;
with Channels;

procedure Sampler_Test is 

   Channel : aliased Channel_ADC_Type (Buffer_Size => 50, 
                              ADC_GPIO => PA0'Access, 
                              ADC_Channel => 0,
                              ADC_Converter => ADC_1'Access);

   S : Channel_ADC.Channel_32bits.Sample;

begin

   USBCOM.Initialize (115_200);
   UART_USB.Transmit_String (USBCOM, "Test");

   Channel.Open_Channel;

   Sampler_Ctrl.Initialize;
   Sampler_Ctrl.Set_Sample_Rate (200);
   Sampler_Ctrl.Set_Channel (Channel'Unchecked_Access);

   begin

      Sampler_Ctrl.Start_Sampling;

      loop
         if Channel.Has_Sample then
            S := Channel.Pop_Sample;
            UART_USB.Transmit_String (USBCOM, S.Value'Image & ASCII.CR & ASCII.LF);
         end if;
      end loop;

      Sampler_Ctrl.Stop_Sampling;

   exception
      when E : Constraint_Error =>
         UART_USB.Transmit_String (USBCOM, Exception_Message (E));
      when E : Program_Error    => 
         UART_USB.Transmit_String (USBCOM, Exception_Message (E));
   end;

end Sampler_Test;