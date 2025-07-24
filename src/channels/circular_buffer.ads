generic
   type Element_Type is private; 
   Buffer_Size : Positive;       
package Circular_Buffer is

   type Circular_Buffer is tagged private;

   procedure Add(Buffer : in out Circular_Buffer; Item : Element_Type);

   function Pop(Buffer : in out Circular_Buffer) return Element_Type;

   procedure Reset(Buffer : in out Circular_Buffer);

   function Is_Empty(Buffer : Circular_Buffer) return Boolean;

   function Is_Full(Buffer : Circular_Buffer) return Boolean;

   function Count(Buffer : Circular_Buffer) return Natural;

   Buffer_Empty_Error : exception;

private
   type Buffer_Array is array (1..Buffer_Size) of Element_Type;

   type Circular_Buffer is tagged record
      Data   : Buffer_Array;
      Head   : Positive := 1; 
      Tail   : Positive := 1;  
      Length : Natural  := 0;  
   end record;
   
end Circular_Buffer;