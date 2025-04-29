with HAL.UART;
with Interfaces;
with System;
with HAL; use HAL;
with UART_USB; use UART_USB; 
with ECGData; 
use type Interfaces.IEEE_Float_32;

-- Testing the python script which embbed an ECG signal into an Ada array.
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

   for I in ECGData.Data'Range loop
      UART_USB.Write16(To_Int16(ECGData.Data(I)), Status);
      delay 1.0 / ECGData.Sample_Rate;
   end loop;

   loop
      null;
   end loop;

end Main;
