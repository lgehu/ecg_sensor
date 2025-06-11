with Ada.Strings; use Ada.Strings;
with Ada.Strings.Bounded;
with Interfaces;

package Commands_Interpreter is

   Max_Arg : constant Positive := 15;

   Commands_Exception : exception; 

   package Command_String is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 32);
   use Command_String;

   subtype Cmd_Str is Command_String.Bounded_String;

   type Command_Type is (ACTION, PARAMETER);

   -- Argument coming from user input
   type Argument is record 
      Key   : Cmd_Str := Command_String.Null_Bounded_String;
      Value : Cmd_Str := Command_String.Null_Bounded_String;
   end record;

   -- Argument stored in the argument pool
   type Abstract_Argument is abstract tagged record
      Key          : Cmd_Str;
      To_String    : access function return String;
      Restore      : access procedure;
      Update_Value : access procedure (Input : String);
      Do_Action    : access procedure (Input : Argument; Valid : Boolean);
   end record; 

   type Arg_Access is access all Abstract_Argument'Class;
   
   type Arg_Array is array (Positive range <>) of Arg_Access;
   
   generic
      type T is private;
      Key : String;
      Default_Value : T;
      Do_Action : access procedure (Input : Argument; Valid : Boolean);
      To_Value : access function (Input : String) return T;
   package Arg_Accessor is

      type Concrete_Argument is new Abstract_Argument with record
         Value : T;
      end record;

      procedure Register;

      procedure Set_Value (Value : T);

      function Is_Registered return Boolean;

      function Get_Default return T;

      function Get_Value return T;

      -- Set current value to default value
      procedure Restore; 

   end Arg_Accessor;

   -- Handle a parameter for discrete type (Works also with Enumeration)
   generic
      type T is (<>);
      Key : String;
      Default_Value : T;
      Action_Fn: access procedure (Arg : Argument; Valid : Boolean);
   package Discrete_Accessor is 

      function To_Value (Input : String) return T;

      --  function To_String return String;

      function Get_Value return T;

      package Accessor is new Arg_Accessor (T => T, 
                                           Key => Key,
                                           Default_Value => Default_Value, 
                                           Do_Action => Action_Fn,
                                           To_Value => To_Value'Access
                                           );
      use Accessor;

      procedure Register;

   end Discrete_Accessor;

   -- Handle a parameter for real type
   generic
      type T is digits <>;
      Key : String;
      Default_Value : T;
      Action_Fn : access procedure (Arg : Argument; Valid : Boolean);
   package Real_Accessor is 

      function To_Value (Input : String) return T;

      package Accessor is new Arg_Accessor (T => T, 
                                             Key => Key, 
                                             Default_Value => Default_Value, 
                                             Do_Action => Action_Fn,
                                             To_Value => To_Value'Access
                                             );
      use Accessor;

      function Get_Value return T;

      procedure Register;
      
   end Real_Accessor;

   -- Handle an action, no value need to be provided.
   generic
      Key : String;
      Action_Fn : access procedure (Input : Argument; Valid : Boolean);
   package Action_Accessor is 

      function Is_Valid (Arg : Argument) return Boolean;

      function To_Value (Input : String) return Boolean;

      function To_String return String;

      package Accessor is new Arg_Accessor (T => Boolean, 
         Key => Key, 
         Default_Value => False, 
         Do_Action => Action_Fn,
         To_Value => To_Value'Access
         );

      use Accessor;

      procedure Register;

   end Action_Accessor;

   function Parse(Input : String; Delimiter : Character := '=') return Argument; 

   function Exist (Key : String) return Boolean;

   function Get_Arg_Count return Natural;

   function Get_Value (Key : String) return String;

   -- Doesn't work with array slice when exceding 3 values . Is a stack error ? No exception is thrown
   -- Old version is: return Arg_Pool ( 1 .. Arg_Len);
   procedure Get_Args (Output : out Arg_Array);

   private 
      function Get_Index (Key : String) return Natural;

end Commands_Interpreter;