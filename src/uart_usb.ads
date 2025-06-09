with STM32.USARTs;          use STM32.USARTs;
with HAL;                   use HAL;
with HAL.UART;              use HAL.UART;
with Ada.Real_Time;         use Ada.Real_Time;
with Ada.Strings.Bounded;
with Interfaces;            use Interfaces;
with Ada.Interrupts;        use Ada.Interrupts;

with STM32.Device;         use STM32.Device;
with STM32.GPIO;           use STM32.GPIO;
with Ada.Interrupts;       use Ada.Interrupts;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;

package UART_USB is

   subtype Int16 is Integer_16;

   package B_Str is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 64);
   use B_Str;

   subtype UART_String is B_Str.Bounded_String;

   type Endianness is (BIG_ENDIAN, LITTLE_ENDIAN); 

   type Command_Read_State is (WAITING, PARSING);
   
   protected type Controller (TX : access GPIO_Point ; RX : access GPIO_Point ; Device : access USART;  IRQ : Interrupt_ID; Timeout_Ms : Natural; Start_Char : Character ; Terminator : Character) is

      procedure Initialize(Baudrate: Baud_Rates := 9600);

      procedure Read_Blocking
         (Data : out UInt9;
         Status: out UART_Status; 
         Timeout : Time_Span := Seconds (1));

      procedure Put_Blocking 
         (Data : UInt9; 
         Status: out UART_Status;
         Timeout : Time_Span := Seconds (1)) ;

      procedure Enable_Interrupt;
   
      procedure Disable_Interrupt;

      function Has_Data return Boolean;

      function Get_Data return UART_String;
   
   private

      procedure Handle_Reception;
   
      procedure IRQ_Handler with Attach_Handler => IRQ;
  
      Raw_Input : UART_String; -- Buffer to store incoming commands

      Command_State : Command_Read_State := WAITING;

      Last_Char_Time : Time := Clock;              -- Last received char from UART

      Is_Data : Boolean := False;

   end Controller;

   generic 
      type T is private;
   procedure Write(This : in out Controller ; Data : T ; Format : Endianness; Status : out UART_Status);

   procedure Transmit_String (This : in out Controller ; Data: String);

   function Receive_String (This : in out Controller ; Delimiter : Character := ASCII.NUL; Timeout : Time_Span := Seconds (1)) return UART_String;

   function Read16(This : in out Controller ; Status : out UART_Status) return Int16;

   procedure Write16(This : in out Controller ; Data: Int16 ; Status: out UART_Status);

end UART_USB;