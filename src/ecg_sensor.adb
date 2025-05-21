with Interfaces; use Interfaces;

with UART_USB;
with PanTompkins;
with AdaData;

procedure Ecg_Sensor is

   Result : IEEE_Float_32;

begin

   UART_USB.Initialize (115_200);
   PanTompkins.Initialize ((Sampling_Frequency => 100.0, 
                           Amplitude_Treshold_Coef => 1.5,
                           Minimal_Pick_Distance_Sec => 0.4,
                           Window_Sec => 0.3,
                           Output_Stage => PanTompkins.Stage_Integrated));

   for I in AdaData.Data'Range loop
      Result := PanTompkins.Process_Sample (AdaData.Data(I));
      UART_USB.Write16(Int16(Result * 1000.0), Status);
      --UART_USB.Transmit_String (Result'Image);
   end loop;

   UART_USB.Write16 (666, Status);

end Ecg_Sensor;