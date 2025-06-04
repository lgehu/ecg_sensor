with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Search;
with Commands_Interpreter;
with UART_USB;
with Ada.Numerics;

package body Commands_Interpreter is

   type Check_Array is array (1 .. Max_Arg) of access function (Input : Argument) return Boolean;

   Arg_Pool : Arg_Array(1 .. Max_Arg) := ( others => (Key => Command_String.Null_Bounded_String,
                                                      Value => Command_String.Null_Bounded_String,
                                                      Default => Command_String.Null_Bounded_String,
                                                      Is_Valid => null,
                                                      Do_Action => null));

   Arg_Len : Natural := 0;

   function Parse(Input: String; Delimiter : Character := '=') return Argument is
      Bounded_Input : Cmd_Str := Command_String.To_Bounded_String (Input);
      Equal_Pos : Natural := Command_String.Index (Bounded_Input, Delimiter & "", 1);
      Tmp, Arg : Argument;
      Find_Arg_Index : Natural := 0;
      Cmd_Type : Command_Type; 
   begin
      
      if Input = "" then
         raise Commands_Exception with "Invalid command";
      end if;

      if Equal_Pos = 0 then
         Cmd_Type := ACTION;
         Tmp.Key := Bounded_Input;
         Tmp.Value := Null_Bounded_String;
      else
         Cmd_Type := PARAMETER;
         Tmp.Key := Command_String.Bounded_Slice(Bounded_Input, 1, Equal_Pos - 1);
         Tmp.Value := Command_String.Bounded_Slice(Bounded_Input, Equal_Pos + 1, Input'Length);
      end if; 

      Arg := Find_Arg (Command_String.To_String (Tmp.Key), Find_Arg_Index);
      
      if Cmd_Type = PARAMETER then

         -- Reset to default value if no value is provided (Ex : ARG=;)
         if Command_String.Length (Tmp.Value) = 0 then
            Arg_Pool (Find_Arg_Index).Value := Arg.Default;
            Arg.Do_Action (Tmp, True);
         elsif Arg.Is_Valid (Tmp) then -- Update value if format is valid
            Arg_Pool (Find_Arg_Index).Value := Tmp.Value;
            Arg.Do_Action (Tmp, True);
         else -- Value is invalid
            Arg.Do_Action (Tmp, False);
         end if;

      else -- No value is provided, perform an action only
         if Arg.Do_Action /= null then
            Arg.Do_Action (Tmp, True);
         end if;
      end if;
      
      return Arg;
   end Parse;

   function Find_Arg (Key : String; Index : out Natural) return Argument is
   begin
      for I in 1 .. Arg_Len loop
         if Command_String.To_String(Arg_Pool (I).Key) = Key then
            Index := I;
            return Arg_Pool (I);
         end if;
      end loop;
      raise Commands_Exception with "Argument not found";
   end Find_Arg;

   function Exist (Key : String) return Boolean is
   begin
      for I in Arg_Pool'Range loop
         if Command_String.To_String(Arg_Pool (I).Key) = Key then
            return True;
         end if;
      end loop;
      return False;
   end;

   package body Arg_Accessor is
      
      Arg_Index : Natural := 0;

      function Check (Input : Argument) return Boolean is
      begin
         if Is_Valid = null then
            return True;
         else
            return Is_Valid (Input);
         end if;
      end Check;

      procedure Execute_Action (Input : Argument; Valid : Boolean) is
      begin
         if Do_Action /= null then
            Do_Action (Input, Valid);
         end if;
      end Execute_Action;

      procedure Check_Registered is
      begin
         if not Is_Registered then
            raise Commands_Exception with "Argument is not registered";
         end if;
      end Check_Registered;

      function Is_Registered return Boolean is
      begin
         return Arg_Index /= 0;
      end Is_Registered;

      procedure Register is
         Index : Natural;
      begin

         if Arg_Len + 1 > Max_Arg then 
            raise Commands_Exception with "Maximum argument reached";
         elsif Is_Registered or Exist (Key) then
            raise Commands_Exception with "The Key " & Key & " already exist"; 
         else
            Arg_Len := Arg_Len + 1;
            Arg_Index := Arg_Len;
            Arg_Pool(Arg_Len) :=  (Key        => Command_String.To_Bounded_String (Key),
                                    Value     => Command_String.To_Bounded_String (Default_Value'Image),
                                    Default   => Command_String.To_Bounded_String (Default_Value'Image), 
                                    Is_Valid  => Check 'Access,
                                    Do_Action => Execute_Action'Access);
         end if;

      end Register;

      procedure Set_Value (Value : T) is 
      begin
         Check_Registered;
         Arg_Pool (Arg_Index).Value := Command_String.To_Bounded_String (Value'Image);
      end;

      function Get_Arg return Argument is
      begin
         Check_Registered;
         return Arg_Pool (Arg_Index);
      end Get_Arg;

      function Get_Raw return Cmd_Str is
      begin
         Check_Registered;
         return Arg_Pool (Arg_Index).Value;
      end Get_Raw;

      function Get_Default return T is
      begin
         return Default_Value;
      end Get_Default;

   end Arg_Accessor;

   package body Discrete_Accessor is

      function Is_Valid (Input : Argument) return Boolean is
      Dummy : T;
      begin
         begin
            Dummy := T'Value (Command_String.To_String (Input.Value));
            return true;
         exception
            when C : Constraint_Error =>
               return false;
         end;
      end;

      procedure Register is
      begin
         Accessor.Register;
      end Register;

      function Get_Value return T is
      begin
         return T'Value (Command_String.To_String (Accessor.Get_Raw));
      end Get_Value;

   end Discrete_Accessor;

   package body Real_Accessor is

      function Is_Valid (Input : Argument) return Boolean is
      Dummy : T;
      begin
         begin
            Dummy := T'Value (Command_String.To_String (Input.Value));
            return true;
         exception
            when C : Constraint_Error =>
               return false;
         end;
      end;

      procedure Register is
      begin
         Accessor.Register;
      end Register;
      
      function Get_Value return T is
      begin
         return T'Value (Command_String.To_String (Accessor.Get_Raw));
      end Get_Value;

   end Real_Accessor;

   package body Action_Accessor is 
      
      function Is_Valid (Arg : Argument) return Boolean is
      begin
         return False;
      end;
      
      procedure Register is 
      begin
         Accessor.Register;
      end;

   end Action_Accessor;

   procedure Get_Args (Output: out Arg_Array)  is
   Min_Len : constant Natural := Natural'Min (Output'Length, Arg_Len);
   begin
      for I in 0 .. Min_Len - 1 loop
         Output (Output'First + I) := Arg_Pool (Arg_Pool'First + I);
      end loop;
   end;

   function Get_Arg_Count return Natural is
   begin
      return Arg_Len;
   end;


end Commands_Interpreter;