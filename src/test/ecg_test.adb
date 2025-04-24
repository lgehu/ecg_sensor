with Ada.Real_Time;
with HAL; use HAL;
with HAL.UART; use HAL.UART;
with UART_USB;

with PanTompkins; use PanTompkins;

procedure Ecg_Sensor is
   Status : UART_Status;
   ECG_Data : Int16;
   Filter : PanTompkins.Filter;
   LP_Result : Int16 := 0;
begin

   UART_USB.Initialize(115_200);
   --UART_USB.Transmit_String ("Hello");
   Filter := PanTompkins.Init_Filter (Fs => 100.0, Fc => 15.0);

   loop
      ECG_Data := UART_USB.Read16(Status);
      if Status = Ok then
         PanTompkins.Lowpass_Filter(Filter, ECG_Data, LP_Result);     
         UART_USB.Write16 (LP_Result, Status);
      end if;
   end loop;


end Ecg_Sensor;