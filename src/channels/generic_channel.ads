with Ada.Real_Time; use Ada.Real_Time;
with HAL;


generic 
   type Value_Type is private;
package Generic_Channel is

   type Sample is record 
      Value          : Value_Type;
      Timestamp      : Time_Span; -- Elapsed time from when channel is open
   end record;

   type Sample_Buffer is array (Positive range <>) of Sample;

   type Generic_Channel_Type (Buffer_Size : Natural) is abstract tagged record 
      Buffer : Sample_Buffer ( 1 .. Buffer_Size);
      Sample_Index : Positive := 1;
      Epoch : Time;
      Open : Boolean;
   end record;

   procedure Reset_Buffer (This : in out Generic_Channel_Type); 
   
   function Pop_Sample (This : in out Generic_Channel_Type) return Sample;

   function Has_Sample (This : in out Generic_Channel_Type) return Boolean;

   procedure Add_Sample (This : in out Generic_Channel_Type ; Value :Value_Type);

end Generic_Channel;