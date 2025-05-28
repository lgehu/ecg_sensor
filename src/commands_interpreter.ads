with Ada.Strings; use Ada.Strings;
with Ada.Strings.Bounded;
with Interfaces;

package Commands_Interpreter is

   Max_Arg : constant Positive := 100;

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

   -- Generic parameter or action builder
   generic
      type T (<>) is private;
      Key : String;
      Default_Value : T;
      Is_Valid : access function (Input : String) return Boolean;
   package Arg_Accessor is

      Arg_Index : Natural := 0;

      procedure Register;

      procedure Set_Value (Value : T);

      function Exist return Boolean;

      function Get_Arg return Argument;

      function Get_Raw return Cmd_Str;

      function Get_Default return T;

   end Arg_Accessor;

   -- Create a parameter for discrete type (Works also with Enumeration)
   generic
      type T is (<>);
      Key : String;
      Default_Value : T;
   package Discrete_Accessor is 
   
      function Is_Valid (Input : String) return Boolean;

      package Accessor is new Arg_Accessor (T => T, 
                                           Key => Key,
                                           Default_Value => Default_Value, 
                                           Cmd_Type => PARAMETER, Is_Valid => Is_Valid'Access
                                           );
      use Accessor;

      procedure Register;

      function Get_Value return T;

   end Discrete_Accessor;

   -- Create a parameter for real type
   generic
      type T is digits <>;
      Key : String;
      Default_Value : T;
   package Real_Accessor is 

      function Is_Valid (Input : String) return Boolean;

      package Accessor is new Arg_Accessor (T => T, 
         Key => Key, 
         Default_Value => Default_Value, 
         Cmd_Type => PARAMETER, Is_Valid => Is_Valid'Access);
      use Accessor;

      procedure Register;
      
      function Get_Value return T;

   end Real_Accessor;

   function Parse(Input : String; Delimiter : Character := '=') return Argument; 

   function Find_Arg (Key : String; Index : out Natural) return Argument;

end Commands_Interpreter;