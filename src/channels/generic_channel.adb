package body Generic_Channel is

   function Is_Open (This : in out Generic_Channel_Type) return Boolean is
   begin
      return This.Open;
   end Is_Open;

   procedure Add_Sample (This : in out Generic_Channel_Type ; Value :Value_Type) is
   begin
      This.Buffer (This.Sample_Index) := (Value => Value, 
                                       Timestamp =>  Clock - This.Epoch);

      if (This.Sample_Index + 1) > This.Buffer'Length then
         This.Buffer (1 .. This.Sample_Index - 1) := This.Buffer (2 .. This.Sample_Index);
      else
         This.Sample_Index := This.Sample_Index + 1;
      end if;

   end Add_Sample;

   function Pop_Sample (This : in out Generic_Channel_Type) return Sample is
   S : Sample;
   begin
      if This.Has_Sample then
         S := This.Buffer (1);
         This.Sample_Index := This.Sample_Index - 1;
      end if;
      return S; 
   end Pop_Sample;

   procedure Reset_Buffer (This : in out Generic_Channel_Type) is 
   begin
      This.Sample_Index := 1;
   end Reset_Buffer;
 
   function Has_Sample (This : in out Generic_Channel_Type) return Boolean is
   Index : Natural := 0;
   begin
      return This.Sample_Index > 1;
   end Has_Sample;

end Generic_Channel;