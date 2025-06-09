procedure Generic_Test is

   protected type Argument(A : T) is
   private
      Key       : Cmd_Str                                              := Command_String.Null_Bounded_String;
      Value     : Cmd_Str                                              := Command_String.Null_Bounded_String;   -- Current stored value
      Default   : Cmd_Str                                              := Command_String.Null_Bounded_String;   -- Default Value
      Is_Valid  : access function (User_Input : Argument) return Boolean    := null; -- Call when a value is provided
      Do_Action : access procedure (User_Input : Argument; Valid : Boolean) := null; -- Always called 
   end Argument;

begin


end Generic_Test;
