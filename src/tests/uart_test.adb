with HAL;         use HAL;
with HAL.UART;    use HAL.UART;

with UART_USB;    use UART_USB;
with Peripherals; use Peripherals;

procedure UART_Test is
   Status : UART_Status;
   Dummy : UART_USB.Int16;
begin

   USBCOM.Initialize (115_200);

   loop
      Dummy := UART_USB.Read16 (USBCOM, Status);
      if Status = Ok then
         UART_USB.Write16 (USBCOM, Dummy, Status);
      end if;
   end loop;

end UART_Test;