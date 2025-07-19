with System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Address_To_Access_Conversions;

package body Channel_Memory is

   package Conv is new System.Address_To_Access_Conversions (Value_Type);
  
   overriding
   procedure Open_Channel (This : in out Channel_Memory_Type) is
   begin
      This.Memory_Offset := 0;
   end Open_Channel;

   overriding
   procedure Close_Channel (This : in out Channel_Memory_Type) is
   begin
      null;
   end Close_Channel;

   overriding
   procedure Read_Channel (This : in out Channel_Memory_Type) is
   Base   : constant System.Address := System'To_Address (This.From_Addr);
   Offset : constant Storage_Offset := Storage_Offset (This.Memory_Offset);
   Ptr : Conv.Object_Pointer := Conv.To_Pointer (Base + Offset * Value_Type'Size / 8);
   begin
      This.Add_Sample (Ptr.all);
      This.Memory_Offset := (This.Memory_Offset + 1) mod This.Length;
   end Read_Channel;

end Channel_Memory;