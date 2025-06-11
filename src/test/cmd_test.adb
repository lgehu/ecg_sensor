with Ada.Real_Time; use Ada.Real_Time;
with Ada.Exceptions; use Ada.Exceptions;
with AdaData;
with Commands_Interpreter; use Commands_Interpreter;
with Interfaces;
with UART_USB; use UART_USB;

procedure Cmd_Test is
   -- TODO : TEST WITH RECORD
   END_FLAG : String := ASCII.LF & ASCII.CR;

   type State is (BUSY, WAITING, PAUSED);

   procedure Log (Msg : String) renames UART_USB.Transmit_String;

   procedure Return_Arg (Arg : Commands_Interpreter.Argument; Valid : Boolean) is
   begin
      if Valid then
         Log (Arg.Key'Image & " = " & Arg.Value'Image & END_FLAG);
      else
         Log ("Invalid parameter" & END_FLAG);
      end if;
   end;

   procedure Print_Args (Input : Commands_Interpreter.Argument; Valid : Boolean) is
   Args : Arg_Array (1 .. Get_Arg_Count);
   begin
      Get_Args (Args);
      for I in Args'Range loop
         Log (Args (I).Key'Image & " = " & Args (I).Value'Image & END_FLAG);
      end loop;
   end Print_Args;

   -- Discrete type example
   package Sample_Rate is new Commands_Interpreter.Discrete_Accessor (T => Integer,
               Key            => "SAMPLE_RATE",
               Default_Value  => 100,
               Action_Fn      => Return_Arg'Access
            );

   -- Enum type example
   package Sensor_State is new Commands_Interpreter.Discrete_Accessor (T => State,
               Key            => "SENSOR_STATE",
               Default_Value  => WAITING,
               Action_Fn      => Return_Arg'Access
            );

   -- Real type example 
   package Amplitude_Coef is new Commands_Interpreter.Real_Accessor (T => Float,
               Key            => "AMPLITUDE_COEF",
               Default_Value  => 1.5,
               Action_Fn      => Return_Arg'Access
            );
 
   -- Action only example
   package Args_Printer is new Commands_Interpreter.Action_Accessor (
               Key => "GET_ARGS", 
               Action_Fn => Print_Args'Access
            );

   -- No callback example
   package No_Callback is new Commands_Interpreter.Discrete_Accessor (T => Natural,
               Key            => "NO_CALLBACK",
               Default_Value  => 42,
               Action_Fn      => null
            );

   Arg : Commands_Interpreter.Argument;
   Input : UART_String;

begin
      UART_USB.Initialize (115_200);
      Log ("Argument interpreter" & END_FLAG);
   
      Sample_Rate.Register;
      Amplitude_Coef.Register;
      Sensor_State.Register;  
      Args_Printer.Register;
      No_Callback.Register;

      loop 
         begin
            Input := UART_USB.Receive_String (ASCII.Semicolon, Time_Span_Last);
            Arg := Commands_Interpreter.Parse (B_Str.To_String (Input));
         exception
            when E : Commands_Interpreter.Commands_Exception =>
                  Log (Exception_Message (E));
         end;
      end loop;

end Cmd_Test;