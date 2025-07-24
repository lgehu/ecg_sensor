package body Circular_Buffer is

   procedure Add(Buffer : in out Circular_Buffer; Item : Element_Type) is
   begin
      if Buffer.Is_Full then
         -- Erase last element when buffer is full
         Buffer.Tail := Buffer.Tail mod Buffer_Size + 1;
         Buffer.Length := Buffer.Length - 1;
      end if;

      Buffer.Data(Buffer.Head) := Item;
      Buffer.Head := Buffer.Head mod Buffer_Size + 1;
      Buffer.Length := Buffer.Length + 1;
   end Add;

   function Pop(Buffer : in out Circular_Buffer) return Element_Type is
      Item : Element_Type;
   begin
      if Buffer.Is_Empty then
         raise Buffer_Empty_Error;
      end if;

      Item := Buffer.Data(Buffer.Tail);
      Buffer.Tail := Buffer.Tail mod Buffer_Size + 1;
      Buffer.Length := Buffer.Length - 1;

      return Item;
   end Pop;

   procedure Reset(Buffer : in out Circular_Buffer) is
   begin
      Buffer.Head := 1;
      Buffer.Tail := 1;
      Buffer.Length := 0;
   end Reset;

   function Is_Empty(Buffer : Circular_Buffer) return Boolean is
   begin
      return Buffer.Length = 0;
   end Is_Empty;

   function Is_Full(Buffer : Circular_Buffer) return Boolean is
   begin
      return Buffer.Length = Buffer_Size;
   end Is_Full;

   function Count(Buffer : Circular_Buffer) return Natural is
   begin
      return Buffer.Length;
   end Count;

end Circular_Buffer;