with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Hash;
with Ada.Exceptions; use Ada.Exceptions;

with UART_USB; use UART_USB;

procedure Map_Test is

   package Integer_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Integer,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   use Integer_Hashed_Maps;

   M : Map;

begin
   UART_USB.Initialize (115_200);
   UART_USB.Transmit_String ("Tu vas marcher oui !? ");

   begin
      M.Insert ("test", 31);

      UART_USB.Transmit_String ("Value: " & Integer'Image(M ("test")));

   exception
      when E : others =>
         UART_USB.Transmit_String ("Exception: " & Exception_Information (E));
   end;

end Map_Test;
