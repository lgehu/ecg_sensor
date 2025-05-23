with Ada.Real_Time; use Ada.Real_Time;

with Commands_Interpreter;

with Interfaces;
with UART_USB; use UART_USB;

procedure Cmd_Test is
   Input : UART_USB.UART_String;
   Cmd : Commands_Interpreter.Command; 

   package Cmd_Str renames Commands_Interpreter.Command_String;

   package Sample_Rate is new Commands_Interpreter.Arg_Desc
            (T         => Integer,
               Name    => Cmd_Str.To_Bounded_String ("SAMPLE_RATE"),
               Default => Cmd_Str.To_Bounded_String ("100"),
               C_Type  => Commands_Interpreter.PARAMETER
            );

begin
   UART_USB.Initialize (115_200);

   loop 
      Input := UART_USB.Receive_String (ASCII.Semicolon, Time_Span_Last);
      Cmd := Commands_Interpreter.Parse (B_Str.To_String (Input));

      declare
         Value : Float;
      begin
         Value := Commands_Interpreter.Get_Value (Cmd, 0.69);
         UART_USB.Transmit_String (Value'Image);
      end;

   end loop;

end Cmd_Test;