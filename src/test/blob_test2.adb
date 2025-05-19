with System;
with UART_USB; use UART_USB;

procedure Main is
   type Blob_Type is array(1 .. 1787) of Character;
   Blob : aliased Blob_Type;
   for Blob'Address use System'To_Address(16#8060000#);
begin
   Initialize(115_200);
   for I in Blob'Range loop
      Transmit_String("" & Blob(I));
   end loop;

   loop
      null;
   end loop;

end Main;