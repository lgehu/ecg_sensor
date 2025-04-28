with HAL.UART;
with System;
with HAL; use HAL;
with System.Storage_Elements; use System.Storage_Elements;
with UART_USB; use UART_USB; 
with DataTest; 

procedure Main is
   -- Déclaration des symboles générés par objcopy
  -- Symboles générés par objcopy (nom basé sur le fichier)
   Text_Start, Text_End, Text_Size: System.Address;
   pragma Import (C, Text_Start, "_binary_test_txt_start");
   pragma Import (C, Text_End,   "_binary_test_txt_end");
   pragma Import (C, Text_Size, "_binary_test_txt_size");

   -- Tableau accédant aux données en flash
   Text_Data : constant Storage_Array (1 .. 63)
      with Import, Address => Text_Start;
   --type Fichier_Type is array (1 .. 63) of Character;
   --pragma Pack (Fichier_Type);
   --Fichier : Fichier_Type;
   --for Fichier'Address use Fichier_Start;

   Status: HAL.UART.UART_Status;

begin
   Initialize;
   Transmit_String ("Hello world !");
   
  -- Transmit_String ("Start: " & Text_Start'Image);
  -- Transmit_String (" End: " & Text_End'Image);

   for I in DataTest.Data'Range loop
      Put_Blocking (UInt9(DataTest.Data(I)), Status);
   end loop;

   loop
      null;
   end loop;

end Main;
