with Interfaces;
with System;
use type Interfaces.IEEE_Float_32;

package ECGData is
   
   type Data_Type is array (Positive range <>) of Interfaces.IEEE_Float_32;

   Data : aliased Data_Type(1 .. 1000);
   for Data'Address use System'To_Address (16#08060000#);  -- Adresse Flash
   -- pragma Import (Ada, Data);                              -- Importation symbolique
   Data_Size   : constant := 1000;
   Sample_Rate : constant := 100;

end ECGData;
