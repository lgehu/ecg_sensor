with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Strings.Search;



package body Commands_Interpreter is

   function Parse(Input: String; Delimiter : Character := '=') return Command is
      Equal_Pos : Natural := Ada.Strings.Search.Index (Input, Delimiter & "", 1);
      Bounded_Input : Cmd_Str := Command_String.To_Bounded_String (Input);
      Cmd : Command;
   begin
      if Equal_Pos = 0 then
         Cmd.Cmd_Type := ACTION;
         Cmd.Name := Bounded_Input;
         Cmd.Value := Null_Bounded_String;
      else
         Cmd.Cmd_Type := PARAMETER;
         Cmd.Name := Command_String.Bounded_Slice(Bounded_Input, 1, Equal_Pos - 1);
         Cmd.Value := Command_String.Bounded_Slice(Bounded_Input, Equal_Pos + 1, Input'Length);
      end if; 

      return Cmd;
   end Parse;

   function Get_Value (Cmd : Command; Default_Value : Float) return Float is
   begin
      if Cmd.Value = Null_Bounded_String then
         return Default_Value;
      else 
         return Float'Value (Command_String.To_String(Cmd.Value));
      end if;
   end Get_Value;

   package body Arg_Desc is
   
      function Get_Value (A : Arg) return T is
      begin
         if A.Cmd_Value = Null_Bounded_String then
            return T'Value (Command_String.To_String (A.Cmd_Default));
         else 
            return T'Value (Command_String.To_String (A.Cmd_Value));
         end if;
      end Get_Value;

   end Arg_Desc;

end Commands_Interpreter;