with HAL; use HAL;
with HAL.UART; use HAL.UART;
with UART_USB; use UART_USB;
with AdaData; 
with Interfaces; use Interfaces; use type Interfaces.IEEE_Float_32;
with PanTompkins; use PanTompkins;

procedure Ecg_Sensor is
   Status : UART_Status;
   BPM : IEEE_Float_32;
   Result : IEEE_Float_32;
begin

   UART_USB.Initialize(115_200);
   PanTompkins.Initialize ((Sampling_Frequency => 100.0, 
                           Amplitude_Treshold_Coef => 1.5,
                           Minimal_Pick_Distance_Sec => 0.4,
                           Window_Sec => 0.3,
                           Output_Stage => PanTompkins.Stage_Derivatived));

   --UART_USB.Transmit_String ("Hello");

   for I in AdaData.Data'Range loop
      Result := PanTompkins.Process_Sample (AdaData.Data(I));
      UART_USB.Write16(Int16(Result * 1000.0), Status);
      --UART_USB.Transmit_String (Result'Image);
   end loop;

   UART_USB.Write16 (-666, Status);

  -- UART_USB.Transmit_String ("End");

   loop
      null;
   end loop;

end Ecg_Sensor;