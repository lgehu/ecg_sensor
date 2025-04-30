with HAL; use HAL;
with HAL.UART; use HAL.UART;
with UART_USB; use UART_USB;
with ECGData;
with Interfaces; use Interfaces; use type Interfaces.IEEE_Float_32;
with PanTompkins; use PanTompkins;

procedure Ecg_Sensor is
   Status : UART_Status;
   BPM : IEEE_Float_32;
   Value : IEEE_Float_32;
begin

   UART_USB.Initialize(115_200);
   PanTompkins.Initialize;

   --UART_USB.Transmit_String ("Hello");

   for I in ECGData.Data'Range loop
      Value := PanTompkins.Process_Sample (ECGData.Data(I));
      BPM := PanTompkins.Get_Heart_Rate;
      UART_USB.Write16(Int16(Value * 1000.0), Status);
   end loop;

   UART_USB.Write16 (-666, Status);

  -- UART_USB.Transmit_String ("End");

   loop
      null;
   end loop;

end Ecg_Sensor;