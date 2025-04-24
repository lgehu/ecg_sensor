-- with HAL; use HAL;
with Interfaces;

package PanTompkins is

   subtype UInt8 is Interfaces.Unsigned_8; 
   subtype Int16 is Interfaces.Integer_16;

   type Filter is record
      Coef: UInt8;
      Last_Value: Int16;
   end record;

   function Init_Filter(Fs: Float; Fc: Float) return Filter;
   procedure Lowpass_Filter(F: in out Filter; Value: Int16; Result: out Int16);

end PanTompkins;