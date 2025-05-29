with Ada.Strings; use Ada.Strings;
with Ada.Strings.Bounded;
with Interfaces;

-- TODO: Store argument in a record that store 2 functions: Is_Valid and Do_Action
-- The first check the validity of the input, the second one perform an action.
package Commands_Interpreter is

   Max_Arg : constant Positive := 100;

   Commands_Exception : exception; 

   package Command_String is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 32);
   use Command_String;

   subtype Cmd_Str is Command_String.Bounded_String;

   type Command_Type is (ACTION, PARAMETER);

   type Argument is record 
      Key       : Cmd_Str      := Command_String.Null_Bounded_String;
      Value     : Cmd_Str      := Command_String.Null_Bounded_String;   -- Current stored value
      Default   : Cmd_Str      := Command_String.Null_Bounded_String;   -- Default Value
      Is_Valid  : access function (Input : Argument) return Boolean;    -- Call when a value is provided
      Do_Action : access procedure (Input : Argument; Valid : Boolean); -- Call if no value is provided
   end record;

   type Arg_Array is array (Natural range <>) of Argument;
   
   -- Generic parameter or action builder
   generic
      type T (<>) is private;
      Key : String;
      Default_Value : T;
      Is_Valid : access function (Input : Argument) return Boolean;
      Do_Action : access procedure (Input : Argument; Valid : Boolean);
   package Arg_Accessor is

      procedure Register;

      procedure Set_Value (Value : T);

      function Is_Registered return Boolean;

      function Get_Arg return Argument;

      function Get_Raw return Cmd_Str;

      function Get_Default return T;

   end Arg_Accessor;

   -- Handle a parameter for discrete type (Works also with Enumeration)
   generic
      type T is (<>);
      Key : String;
      Default_Value : T;
      Action_Fn: access procedure (Arg : Argument; Valid : Boolean);
   package Discrete_Accessor is 
   
      function Is_Valid (Input : Argument) return Boolean;

      package Accessor is new Arg_Accessor (T => T, 
                                           Key => Key,
                                           Default_Value => Default_Value, 
                                           Is_Valid => Is_Valid'Access,
                                           Do_Action => Action_Fn
                                           );
      use Accessor;

      procedure Register;

      function Get_Value return T;

   end Discrete_Accessor;

   -- Handle a parameter for real type
   generic
      type T is digits <>;
      Key : String;
      Default_Value : T;
      Action_Fn : access procedure (Arg : Argument; Valid : Boolean);
   package Real_Accessor is 

      function Is_Valid (Input : Argument) return Boolean;

      package Accessor is new Arg_Accessor (T => T, 
         Key => Key, 
         Default_Value => Default_Value, 
         Is_Valid => Is_Valid'Access,
         Do_Action => Action_Fn);
      use Accessor;

      procedure Register;
      
      function Get_Value return T;

   end Real_Accessor;

   -- Handle an action, no value need to be provided.
   generic
      Key : String;
      Action_Fn : access procedure (Input : Argument; Valid : Boolean);
   package Action_Accessor is 

      function Is_Valid (Arg : Argument) return Boolean;

      package Accessor is new Arg_Accessor (T => Integer, 
         Key => Key, 
         Default_Value => 0, 
         Is_Valid => Is_Valid'Access,
         Do_Action => Action_Fn);
      use Accessor;

      procedure Register;

   end Action_Accessor;

   function Parse(Input : String; Delimiter : Character := '=') return Argument; 

   function Find_Arg (Key : String; Index : out Natural) return Argument;

   function Exist (Key : String) return Boolean;

   function Get_Args return Arg_Array;

end Commands_Interpreter;