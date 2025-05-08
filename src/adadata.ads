
with Interfaces; use Interfaces;
with System;
use type Interfaces.IEEE_Float_32;

-- This file was generated with to_ada.py
-- File from ../ecg_sensor/physionet.org/files/ptb-xl/1.0.3/records100/00000/00003_lr
package AdaData is
   --pragma Elaborate_Body;
    type Data_Type is array (Positive range <>) of IEEE_Float_32;

    Data : aliased Data_Type(1 .. 1000);
    for Data'Address use System'To_Address (16#08060000#);
   
    Data_Size   : constant Positive := 1000;
    Sample_Rate : constant Positive := 100;

end AdaData;
                