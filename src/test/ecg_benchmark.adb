with HAL;                   use HAL;
with HAL.UART;
with Interfaces;            use Interfaces;
with Ada.Real_Time;         use Ada.Real_Time;
with Ada.Execution_Time;    use Ada.Execution_Time;
use type Interfaces.IEEE_Float_32;

with UART_USB;
with PanTompkins; 
with AdaData;               use AdaData;
with Peripherals;           use Peripherals;

-- Testing generated ECG data by to_ada.py
procedure Main is

   Status: HAL.UART.UART_Status;
   Result : IEEE_Float_32;
   Start_Time : CPU_Time;
   Elapsed_Time : Duration;
begin

   USBCOM.Initialize (115_200);

   PanTompkins.Initialize ((Sampling_Frequency => 500, 
                           Amplitude_Treshold_Coef => 1.5,
                           Minimal_Pick_Distance_Sec => 0.4,
                           Window_Sec => 0.3,
                           Output_Stage => PanTompkins.Stage_Derivatived));

   UART_USB.Transmit_String(USBCOM, "Data adress:" & AdaData.Data'Address'Image & ASCII.LF & ASCII.CR);

   loop
      for I in AdaData.Data'Range loop
         Start_Time := Clock;
         Result := PanTompkins.Process_Sample (AdaData.Data(I));
         Elapsed_Time := To_Duration(Clock - Start_Time) * 1_000.0;
         UART_USB.Transmit_String (USBCOM, Elapsed_Time'Image & " ms" & ASCII.LF & ASCII.CR);
      end loop;
   end loop;
   -- Results: 
   -- without optimisation : ~32 us
   -- With optimization (-02): ~8 us
end Main;
