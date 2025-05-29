with Ada.Real_Time; use Ada.Real_Time;
with Ada.Exceptions; use Ada.Exceptions;
with AdaData;
with Commands_Interpreter; use Commands_Interpreter;
with Interfaces;
with UART_USB; use UART_USB;

procedure Cmd_Test is

   END_FLAG : String := ASCII.LF & ASCII.CR;

   type State is (BUSY, WAITING, PAUSED);

   procedure Log (Msg : String) renames UART_USB.Transmit_String;

   procedure Return_Arg (Arg : Commands_Interpreter.Argument; Valid : Boolean) is
   begin
      if Valid then
         UART_USB.Transmit_String (Arg.Key'Image & " = " & Arg.Value'Image & END_FLAG);
      else
         UART_USB.Transmit_String ("Invalid parameter" & END_FLAG);
      end if;
   end;

   procedure Print_Args (Input : Commands_Interpreter.Argument; Valid : Boolean) is
   begin
      begin
         for Arg of Commands_Interpreter.Get_Args loop
            UART_USB.Transmit_String (Arg.Key'Image & " = " & Arg.Value'Image & END_FLAG);
         end loop;
      exception
         when E : Program_Error =>
            UART_USB.Transmit_String (Exception_Message (E));
      end;
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
               Default_Value  => 32,
               Action_Fn      => null
            );

   Arg : Commands_Interpreter.Argument;
   Input : UART_String;

begin
      UART_USB.Initialize (115_200);
      UART_USB.Transmit_String ("Argument interpreter" & END_FLAG);
   
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
                  UART_USB.Transmit_String (Exception_Message (E));
         end;
      end loop;

end Cmd_Test;