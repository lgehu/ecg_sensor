with vus_functions;

package VUS is

   type VUS_State is limited private;
   type VUS_Label is (Silent, Unvoiced, Voiced);

   Frame_Length : constant := 480;
   Hop_Size     : constant := 160;

   subtype Frame_Range is Natural range 0 .. Frame_Length - 1;
   type Float_Array is array (Frame_Range) of Float;

   procedure Initialize (State : in out VUS_State; Init_Signal : in Float_Array);

   procedure VUS_Compute_Frame (
      State  : in out VUS_State;
      Frame  : in     Float_Array;
      Result :    out VUS_Label
   );

private
   type VUS_State is record
      Window         : vus_functions.Float_Array (Frame_Range);
      E_Threshold    : Float := 0.0;
      ZCR_Threshold  : Float := 0.0;
   end record;

end VUS;
