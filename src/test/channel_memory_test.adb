with Channel_ADC; use Channel_ADC;
with Channel_Memory;
with Peripherals; use Peripherals;
with STM32.Device; use STM32.Device;
with UART_USB;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces; use Interfaces;

procedure Channel_Memory_Test is

   package Flash_Float is new Channel_Memory (Value_Type => IEEE_Float_32);

   Channel : Flash_Float.Channel_Memory_Type (Buffer_Size => 50, From_Addr => 16#8060000#, Length => 1000);
   S : Flash_Float.Channel_Gen.Sample;

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

end Channel_Memory_Test;