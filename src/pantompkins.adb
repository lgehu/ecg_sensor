with Ada.Numerics;         use Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;

package body PanTompkins is 

   function Init_Filter(Fs: Float; Fc: Float) return Filter is
   Ts : constant Float := 1.0 / Fs; 
   Alpha : constant Float := Ts / (Ts + 1.0 / (2.0 * Pi * Fc));
   begin
      Put(Alpha'Image);
      return (UInt8(Alpha * 255.0), 0);
   end Init_Filter;

   procedure Lowpass_Filter(F: in out Filter; Value: Int16; Result: out Int16) is
   Last_Value : constant Int16 := F.Last_Value;
   A : constant Integer := Integer(F.Coef) * Integer(Value);
   B : constant Integer := (256 - Integer(F.Coef)) * Integer(Last_Value);
   begin
      F.Last_Value := Value;
      Result := Int16((A + B) / 256);
   end Lowpass_Filter;

end PanTompkins;