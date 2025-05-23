with Ada.Strings; use Ada.Strings;
with Ada.Strings.Bounded;
with Interfaces;

package Commands_Interpreter is

   package Command_String is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 32);
   use Command_String;
   subtype Cmd_Str is Command_String.Bounded_String;

   type Command_Type is (ACTION, PARAMETER);

   generic
      type T is (<>);
      Name : Cmd_Str;
      Default : Cmd_Str;
      C_Type : Command_Type;
   package Arg_Desc is

      type Arg is record 
         Cmd_Name        : Cmd_Str := Name;
         Cmd_Default     : Cmd_Str := Default;
         Cmd_Value       : Cmd_Str; -- Current stored value
         Cmd_Data_Type   : Command_Type := C_Type;
      end record;

      function Get_Value (A : Arg) return T;

   end Arg_Desc;
     
   type Arg_Pool is array (Natural range 1 .. 10) of Arg_Desc.Arg;

   type Command is record
      Cmd_Type : Command_Type;
      Value : Cmd_Str;
      Default_Value : Cmd_Str;
      Name : Cmd_Str;
   end record;


   function Get_Value (Cmd : Command; Default_Value : Float) return Float;


   --Command_Pool : array (Natural range 1 .. Max_Cmd) of Command;

   function Parse(Input : String; Delimiter : Character := '=') return Command; 


end Commands_Interpreter;