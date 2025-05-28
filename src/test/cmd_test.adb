with Ada.Real_Time; use Ada.Real_Time;
with Ada.Exceptions; use Ada.Exceptions;
with AdaData;
with Commands_Interpreter;

with Interfaces;
with UART_USB; use UART_USB;

procedure Cmd_Test is

   type State is (BUSY, WAITING, PAUSED);

   package Sample_Rate is new Commands_Interpreter.Discrete_Accessor (T => Integer,
               Key            => "SAMPLE_RATE",
               Default_Value  => 100
            );

   package Sensor_State is new Commands_Interpreter.Discrete_Accessor (T => State,
               Key            => "SENSOR_STATE",
               Default_Value  => WAITING
            );

   package Amplitude_Coef is new Commands_Interpreter.Real_Accessor (T => Float,
               Key            => "AMPLITUDE_COEF",
               Default_Value  => 1.5
            );

   Arg : Commands_Interpreter.Argument;
   Input : UART_String;

begin
   begin
      UART_USB.Initialize (115_200);
      UART_USB.Transmit_String ("Argument interpreter" & ASCII.LF & ASCII.CR);
      
      Sample_Rate.Register;
      Amplitude_Coef.Register;
      Sensor_State.Register;

      loop 
         Input := UART_USB.Receive_String (ASCII.Semicolon, Time_Span_Last);
         Arg := Commands_Interpreter.Parse (B_Str.To_String (Input));
         
         UART_USB.Transmit_String (Sample_Rate.Accessor.Get_Arg'Image & ASCII.LF & ASCII.CR);
         UART_USB.Transmit_String (Amplitude_Coef.Accessor.Get_Arg'Image & ASCII.LF & ASCII.CR);
         UART_USB.Transmit_String (Sensor_State.Accessor.Get_Arg'Image & ASCII.LF & ASCII.CR);

      end loop;
   exception
      when E : Commands_Interpreter.Commands_Exception =>
            UART_USB.Transmit_String (Exception_Message (E));
      when C : Constraint_Error =>
            UART_USB.Transmit_String (Exception_Message (C)); 
   end;

end Cmd_Test;