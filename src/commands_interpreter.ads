with Ada.Strings; use Ada.Strings;
with Ada.Strings.Bounded;
with Interfaces;

package Commands_Interpreter is

   Commands_Exception : exception; 

   package Command_String is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 32);
   use Command_String;

   type Convert_Func is access function (Input : String) return Boolean;

   subtype Cmd_Str is Command_String.Bounded_String;

   type Command_Type is (ACTION, PARAMETER);

   type Argument is record 
      Key       : Cmd_Str;
      Cmd_Type  : Command_Type;
      Value     : Cmd_Str; -- Current stored value
   end record;

   generic
      type T (<>) is private;
      Key : String;
      Default_Value : T;
      Cmd_Type : Command_Type;
      Is_Valid : access function (Input : String) return Boolean;
      --with function Is_Valid (Input : String) return Boolean;
   package Arg_Accessor is

      Arg_Index : Natural := 0;

      procedure Register;

      procedure Set_Value (Value : T);

      function Exist return Boolean;

      function Get_Arg return Argument;

      function Get_Raw return Cmd_Str;

   end Arg_Accessor;

   generic
      type T is (<>);
      Key : String;
      Default_Value : T;
      Cmd_Type : Command_Type;
   package Discrete_Accessor is 
   
      function Is_Valid (Input : String) return Boolean;

      package Accessor is new Arg_Accessor (T => T, 
                                           Key => Key,
                                           Default_Value => Default_Value, 
                                           Cmd_Type => Cmd_Type, Is_Valid => Is_Valid'Access
                                           );
      use Accessor;

      procedure Register;

      function Get_Value return T;

   end Discrete_Accessor;

   generic
      type T is digits <>;
      Key : String;
      Default_Value : T;
      Cmd_Type : Command_Type;
   package Real_Accessor is 

      function Is_Valid (Input : String) return Boolean;

      package Accessor is new Arg_Accessor (T => T, 
         Key => Key, 
         Default_Value => Default_Value, 
         Cmd_Type => Cmd_Type, Is_Valid => Is_Valid'Access);
      use Accessor;

      procedure Register;
      
      function Get_Value return T;

   end Real_Accessor;

   function Parse(Input : String; Delimiter : Character := '=') return Argument; 

   function Find_Arg (Key : String; Index : out Natural) return Argument;

end Commands_Interpreter;