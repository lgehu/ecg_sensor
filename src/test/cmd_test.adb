with Ada.Real_Time; use Ada.Real_Time;
with Ada.Exceptions; use Ada.Exceptions;
with AdaData;
with Commands_Interpreter; use Commands_Interpreter;
with Interfaces;
with UART_USB; use UART_USB;

procedure Cmd_Test is

   END_FLAG : String := ASCII.LF & ASCII.CR;

   type State is (BUSY, WAITING, PAUSED);

   -- Discrete type example
   package Sample_Rate is new Commands_Interpreter.Discrete_Accessor (T => Integer,
               Key            => "SAMPLE_RATE",
               Default_Value  => 100
            );

   -- Enum type example
   package Sensor_State is new Commands_Interpreter.Discrete_Accessor (T => State,
               Key            => "SENSOR_STATE",
               Default_Value  => WAITING
            );

   -- Real type example 
   package Amplitude_Coef is new Commands_Interpreter.Real_Accessor (T => Float,
               Key            => "AMPLITUDE_COEF",
               Default_Value  => 1.5
            );

   function Print_Args (Input : Commands_Interpreter.Argument) return Boolean is
      Value : String := Commands_Interpreter.Command_String.To_String (Input.Value);
      Index : Natural;
   begin
      begin
         if Value = "ALL" or Value = "" then
            -- In this case, 'Image doesn't work while iterating 
            for Arg of Commands_Interpreter.Get_Args loop
               UART_USB.Transmit_String (Arg.Key'Image & " = " & Arg.Value'Image & END_FLAG);
            end loop;
         else
            UART_USB.Transmit_String (Commands_Interpreter.Find_Arg (Value, Index)'Image & END_FLAG);
         end if;
      exception
         when E : Constraint_Error =>
            UART_USB.Transmit_String (Exception_Message (E));
         when E : Commands_Exception =>
            UART_USB.Transmit_String (Exception_Message (E));
         when E : Program_Error =>
            UART_USB.Transmit_String (Exception_Message (E));
      end;
      return True;
   end Print_Args;
   
   package Args_Printer is new Commands_Interpreter.Action_Accessor (Key => "GET_ARG", Action_Fn => Print_Args'Access);

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