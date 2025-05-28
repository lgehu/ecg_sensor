with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Search;
with Commands_Interpreter;
with UART_USB;
with Ada.Numerics;

package body Commands_Interpreter is

   type Arg_Array is array (Positive range <>) of Argument;

   type Check_Array is array (1 .. 100) of access function (Input : String) return Boolean;

   Arg_Pool : Arg_Array(1 .. 100);
   Check_Pool : Check_Array;

   Arg_Len : Natural := 0;

   function Parse(Input: String; Delimiter : Character := '=') return Argument is
      Equal_Pos : Natural := Ada.Strings.Search.Index (Input, Delimiter & "", 1);
      Bounded_Input : Cmd_Str := Command_String.To_Bounded_String (Input);
      Arg, Dummy : Argument;
      Arg_Index : Natural := 0;
   begin
      if Equal_Pos = 0 then
         Arg.Cmd_Type := ACTION;
         Arg.Key := Bounded_Input;
         Arg.Value := Null_Bounded_String;
      else
         Arg.Cmd_Type := PARAMETER;
         Arg.Key := Command_String.Bounded_Slice(Bounded_Input, 1, Equal_Pos - 1);
         Arg.Value := Command_String.Bounded_Slice(Bounded_Input, Equal_Pos + 1, Input'Length);
      end if; 

      Dummy := Find_Arg (Command_String.To_String (Arg.Key), Arg_Index);
      
      -- Update value only if the format is valid
      if Check_Pool (Arg_Index) (Command_String.To_String (Arg.Value)) then
         Arg_Pool (Arg_Index).Value := Arg.Value;
      end if;
      
      return Arg;
   end Parse;

   function Find_Arg (Key : String; Index : out Natural) return Argument is
   begin
      for I in Arg_Pool'Range loop
         if Command_String.To_String(Arg_Pool (I).Key) = Key then
            Index := I;
            return Arg_Pool (I);
         end if;
      end loop;
      raise Commands_Exception with "Argument not found";
   end Find_Arg;


   package body Arg_Accessor is
      
      use Commands_Interpreter;

      procedure Check_Registered is
      begin
         if not Exist then
            raise Commands_Exception with "Argument is not registered";
         end if;
      end Check_Registered;

      function Exist return Boolean is
      begin
         return Arg_Index > 0 and Arg_Index <= Arg_Len;
      end Exist;

      procedure Register is
      begin
         if not Exist then

            Arg_Len := Arg_Len + 1;
            Arg_Index := Arg_Len;
            Arg_Pool(Arg_Len) :=  (Key     => Command_String.To_Bounded_String (Key),
                                    Value    => Command_String.To_Bounded_String (Default_Value'Image),
                                    Cmd_Type => Cmd_Type);

           -- Check_Pool (Arg_Len) := Valider'Access;
          -- UART_USB.Transmit_String (Arg_Index'Image & " " & Arg_Len'Image);

         else
            raise Commands_Exception with "This argument already exist";
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

   end Arg_Accessor;

   package body Discrete_Accessor is

      function Dummy_Fun (Input : String) return Boolean is
      begin
         return Valid_Fn (Input);
      end;

      procedure Register is
      begin
         Accessor.Register;
         Check_Pool (Arg_Len) := Dummy_Fun'Access;
      end Register;

      function Get_Value return T is
      begin
         return T'Value (Command_String.To_String (Accessor.Get_Raw));
      end Get_Value;


   end Discrete_Accessor;

   package body Real_Accessor is

      function Dummy_Fun (Input : String) return Boolean is
      begin
         return Valid_Fn (Input);
      end;

      procedure Register is
      begin
         Accessor.Register;
         Check_Pool (Arg_Len) := Dummy_Fun'Access;
      end Register;
      
      function Get_Value return T is
      begin
         return T'Value (Command_String.To_String (Accessor.Get_Raw));
      end Get_Value;

   end Real_Accessor;

   function Real_Is_Valid (Input : String) return Boolean is
      Dummy : T;
      begin
         begin
            Dummy := T'Value (Input);
            UART_USB.Transmit_String ("ok");
            return true;
         exception
            when C : Constraint_Error =>
               UART_USB.Transmit_String (Exception_Message (C));
               return false;
         end;
   end Real_Is_Valid;

   function Discrete_Is_Valid (Input : String) return Boolean is
   Dummy : T;
   begin
      begin
         Dummy := T'Value (Input);
         return true;
      exception
         when C : Constraint_Error =>
            return false;
      end;
   end Discrete_Is_Valid;
   
end Commands_Interpreter;