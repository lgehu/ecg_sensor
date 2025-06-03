with STM32.USARTs; use STM32.USARTs;
with HAL; use HAL;
with HAL.UART; use HAL.UART;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Bounded;
with Interfaces; use Interfaces;

package UART_USB is

   subtype Int16 is Integer_16;

   package B_Str is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 64);
   use B_Str;

   subtype UART_String is B_Str.Bounded_String;

   type Endianness is (BIG_ENDIAN, LITTLE_ENDIAN); 

   procedure Initialize(Baudrate: Baud_Rates := 9600);

   procedure Read_Blocking
      (Data : out UInt9;
      Status: out UART_Status; 
      Timeout : Time_Span := Seconds (1));

   procedure Put_Blocking 
      (Data : UInt9; 
      Status: out UART_Status;
      Timeout : Time_Span := Seconds (1)) ;

   function Read16(Status: out UART_Status) return Int16;
   procedure Write16(Data: Int16; Status : out UART_Status);

   procedure Transmit_String (Data: String);

   function Receive_String (
      Delimiter: Character := ASCII.NUL;
      Timeout : Time_Span := Seconds (1)) return UART_String;

   generic 
      type T is private;
   procedure Write(Data : T; Format : Endianness; Status : out UART_Status);
   --with
    --  Pre => T'Size mod 8 = 0;

end UART_USB;