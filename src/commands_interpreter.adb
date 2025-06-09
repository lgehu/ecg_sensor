with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Search;
with Commands_Interpreter;
with UART_USB;
with Ada.Numerics;

package body Commands_Interpreter is

 
   Arg_Pool : Arg_Array (1 .. Max_Arg);
   Arg_Len : Natural := 0;

   function Parse(Input: String; Delimiter : Character := '=') return Argument is
      Bounded_Input : Cmd_Str := Command_String.To_Bounded_String (Input);
      Equal_Pos : Natural := Command_String.Index (Bounded_Input, Delimiter & "", 1);
      Tmp : Argument;
      Arg_Index : Natural := 0;
      Cmd_Type : Command_Type;
      Arg : Arg_Access;
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

      Arg_Index := Get_Index (Command_String.To_String (Tmp.Key));
      Arg := Arg_Pool (Arg_Index);

      if Arg = null then 
         return Tmp;
      end if;

      if Cmd_Type = PARAMETER then
         -- Reset to default value if no value is provided (Ex : ARG=;)
         if Command_String.Length (Tmp.Value) = 0 then
            Arg.Restore.all;
            Arg.Do_Action (Tmp, True);
         else
            begin
               -- Try to update value
               Arg.Update_Value (Command_String.To_String (Tmp.Value));
               Arg.Do_Action (Tmp, True);
            exception 
               when Constraint_Error =>
                  Arg.Do_Action (Tmp, False);
            end;
         end if;
      else -- No value is provided, perform an action only
         Arg.Do_Action (Tmp, True);
      end if;
      
      return Tmp;
   end Parse;

   function Get_Index (Key : String) return Natural is
   begin
      for I in 1 .. Arg_Len loop
         if Command_String.To_String(Arg_Pool (I).Key) = Key then
            return I;
         end if;
      end loop;
      raise Commands_Exception with "Argument " & Key & " not found";
   end Get_Index;

   function Exist (Key : String) return Boolean is
   begin
      UART_USB.Transmit_String (Key & ASCII.CR & ASCII.LF);
      for I in 1 .. Arg_Len loop
         if Command_String.To_String(Arg_Pool (I).Key) = Key then
            return True;
         end if;
      end loop;
      return False;
   end;

   function Get_Value (Key : String) return String is
   Index : Natural := Get_Index (Key);
   begin
      return Arg_Pool (Index).To_String.all;
   end Get_Value;

   package body Arg_Accessor is
      
      -- Arg : access Concrete_Argument'Class := null;
      -- Fast access
      Arg_Index : Natural := 0;

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
   
      procedure Update_Value (Input : String) is
      begin
         Check_Registered;
         declare
            Arg : access Concrete_Argument'Class := Concrete_Argument (Arg_Pool (Arg_Index).all)'Access;
         begin
            if To_Value /= null then
               Arg.Value := To_Value (Input);
            end if;
         end;
      end Update_Value;

      procedure Restore is
      begin
         Check_Registered;
         declare
            Arg : Concrete_Argument'Class := Concrete_Argument (Arg_Pool (Arg_Index).all);
         begin
            Arg.Value := Default_Value;
         end;
      end Restore;

      function Is_Registered return Boolean is
      begin
         return Arg_Index /= 0;
      end Is_Registered;

      function To_String return String is
      begin
         return Get_Value'Image;
      end To_String;

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
            Arg_Pool(Arg_Len) := new Concrete_Argument'(Key => Command_String.To_Bounded_String (Key), 
                                                         Value => Default_Value,
                                                         To_String => To_String'Access,
                                                         Restore => Restore'Access,
                                                         Do_Action => Execute_Action'Access,
                                                         Update_Value => Update_Value'Access);
         end if;

      end Register;

      procedure Set_Value (Value : T) is 
      begin
         Check_Registered;
         Arg_Pool (Arg_Index).Update_Value (Value'Image);
      end;

      function Get_Default return T is
      begin
         return Default_Value;
      end Get_Default;

      function Get_Value return T is
      begin
         Check_Registered;
         declare
            Arg : Concrete_Argument'Class := Concrete_Argument (Arg_Pool (Arg_Index).all);
         begin
            return Arg.Value;
         end;
      end Get_Value;

   end Arg_Accessor;

   package body Discrete_Accessor is

      function To_Value (Input : String) return T is
      begin
         return T'Value (Input);
      end To_Value;

      procedure Register is
      begin
         Accessor.Register;
      end Register;

      function Get_Value return T is
      begin
         return Accessor.Get_Value;
      end Get_Value;

   end Discrete_Accessor;

   package body Real_Accessor is

      function To_Value (Input : String) return T is
      begin
         return T'Value (Input);
      end To_Value;

      procedure Register is
      begin
         Accessor.Register;
      end Register;

      function Get_Value return T is
      begin
         return Accessor.Get_Value;
      end Get_Value;
      
   end Real_Accessor;

   package body Action_Accessor is 

      function To_Value (Input : String) return Boolean is
      begin
         return False;
      end To_Value;
      
      function Is_Valid (Arg : Argument) return Boolean is
      begin
         return False;
      end;

      function To_String return String is
      begin
         return "";
      end To_String;
      
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