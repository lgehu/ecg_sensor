with Ada.Real_Time; use Ada.Real_Time;
with Ada.Exceptions; use Ada.Exceptions;
with AdaData;
with Commands_Interpreter; use Commands_Interpreter;
with Interfaces;
with UART_USB; use UART_USB;

procedure Cmd_Test is

   END_FLAG : String := ASCII.LF & ASCII.CR;

   type State is (BUSY, WAITING, PAUSED);

   procedure Return_Arg (Arg : Commands_Interpreter.Argument) is
   begin
      UART_USB.Transmit_String (Arg'Image & END_FLAG);
   end;

   procedure Print_Args (Input : Commands_Interpreter.Argument) is
      Value : String := Commands_Interpreter.Command_String.To_String (Input.Value);
      Index : Natural;
   begin
      for Arg of Commands_Interpreter.Get_Args loop
         UART_USB.Transmit_String (Arg.Key'Image & " = " & Arg.Value'Image & END_FLAG);
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
 
   package Args_Printer is new Commands_Interpreter.Action_Accessor (
               Key => "GET_ARGS", 
               Action_Fn => Print_Args'Access
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