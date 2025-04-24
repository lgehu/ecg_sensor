with HAL; use HAL;
with HAL.UART; use HAL.UART;
with UART_USB;

procedure Ecg_Sensor is
   Status : UART_Status;
   Dummy : UART_USB.Int16;
begin

   UART_USB.Initialize(115_200);
   --UART_USB.Transmit_String ("Hello");

   loop
      Dummy := UART_USB.Read16(Status);
      if Status = Ok then
         UART_USB.Write16 (Dummy, Status);
      end if;
   end loop;

end Ecg_Sensor;