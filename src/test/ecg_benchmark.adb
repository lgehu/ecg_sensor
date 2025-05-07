with UART_USB;
with PanTompkins; 
with ECGData; use ECGData;
with HAL; use HAL;
with HAL.UART;
with Interfaces; use Interfaces;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Execution_Time; use Ada.Execution_Time;
use type Interfaces.IEEE_Float_32;

-- Testing generated ECG data by to_ada.py
procedure Main is

   Status: HAL.UART.UART_Status;
   Result : IEEE_Float_32;
   Start_Time : CPU_Time;
   Elapsed_Time : Duration;
begin
   UART_USB.Initialize(115_200);
   PanTompkins.Initialize (100.0);

   loop
      for I of ECGData.Data loop
         Start_Time := Clock;
         Result := PanTompkins.Process_Sample (I);
         Elapsed_Time := To_Duration(Clock - Start_Time) * 1_000.0;
         UART_USB.Transmit_String (Elapsed_Time'Image & " ms" & ASCII.LF & ASCII.CR);
      end loop;
   end loop;
   -- Results: 
   -- without optimisation : ~32 us
   -- With optimization (-02): ~8 us
end Main;
