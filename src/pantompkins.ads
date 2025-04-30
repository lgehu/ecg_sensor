with Interfaces; use Interfaces;

package PanTompkins is
   type Sample_Array is array (Natural range <>) of IEEE_Float_32;

   procedure Initialize;
   function Process_Sample(Sample : IEEE_Float_32) return IEEE_Float_32;
   function Get_Heart_Rate return IEEE_Float_32;
   
end PanTompkins;
