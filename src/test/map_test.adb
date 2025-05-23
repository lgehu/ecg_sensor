with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Hash;
with Ada.Exceptions; use Ada.Exceptions;

with UART_USB; use UART_USB;


-- Map seems to not work
procedure Map_Test is

   M : Map;

begin
   UART_USB.Initialize (115_200);

   begin
      M.Insert ("test", 31);

      UART_USB.Transmit_String ("Value: " & Integer'Image(M ("test")));

   exception
      when E : others =>
         UART_USB.Transmit_String ("Exception: " & Exception_Information (E));
   end;

end Map_Test;
