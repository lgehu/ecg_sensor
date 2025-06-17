
with Interfaces; use Interfaces;
with System;
use type Interfaces.IEEE_Float_32;

--- This file was generated with to_ada.py
-- File from ecg_data_1KHz.mat
package AdaData is
   type Data_Type is array (Positive range <>) of IEEE_Float_32;

   Data : aliased Data_Type(1 .. 12000);
   for Data'Address use System'To_Address (16#08060000#);
   
   Data_Size   : constant Positive := 1000;
   Sample_Rate : constant Positive := 1000;

end AdaData;