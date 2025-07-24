package Channels is

   type Channel is interface;

   procedure Open_Channel (This : in out Channel) is abstract;

   procedure Close_Channel (This : in out Channel) is abstract;

   procedure Read_Channel (This : in out Channel) is abstract;

   function Is_Open (This : in out Channel) return Boolean is abstract;

end Channels;