with HAL.UART;
with Interfaces;
with System;
with HAL; use HAL;
with UART_USB; use UART_USB; 
with AdaData; 
use type Interfaces.IEEE_Float_32;

-- Testing generated ECG data by to_ada.py
procedure Main is

   Status: HAL.UART.UART_Status;

   -- Convert ECG values to Int16 for UART transfer
   function To_Int16(Value: Interfaces.IEEE_Float_32) return Int16 is
   begin
      return UART_USB.Int16(Value * 1000.0);
   end To_Int16;   

begin
   Initialize(115_200);
   Transmit_String ("Beginning transfer...");

   for I in 1 .. AdaData.Data_Size loop
      UART_USB.Transmit_String(AdaData.Data(I)'Image & ASCII.LF & ASCII.CR);
      -- UART_USB.Write16(UART_USB.Int16(I), Status);
   end loop;

   loop
      null;
   end loop;

end Main;
