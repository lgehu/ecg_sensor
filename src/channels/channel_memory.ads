with Generic_Channel; 
with Channels;

generic 
   type Value_Type is private;
package Channel_Memory is

   package Channel_Gen is new Generic_Channel (Value_Type => Value_Type);
  
   type Channel_Memory_Type (Buffer_Size : Natural ; From_Addr : Natural ; Length : Natural)
   is new Channel_Gen.Generic_Channel_Type (Buffer_Size) and Channels.Channel with record
      Memory_Offset : Natural;
   end record;

   overriding
   procedure Open_Channel (This : in out Channel_Memory_Type);

   overriding
   procedure Close_Channel (This : in out Channel_Memory_Type);

   overriding
   procedure Read_Channel (This : in out Channel_Memory_Type);

   overriding
   function Is_Open (This : in out Channel_Memory_Type) return Boolean;

end Channel_Memory;