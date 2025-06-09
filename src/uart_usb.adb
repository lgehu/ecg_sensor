with STM32.GPIO;    use STM32.GPIO;
with STM32.Device;  use STM32.Device;
with Ada.Unchecked_Conversion;
with Peripherals; use Peripherals;

package body UART_USB is

   procedure Initialize(Baudrate: Baud_Rates := 9600) is 
   begin
      Enable_Clock (Transceiver);
      Enable_Clock (RX_Pin & TX_Pin);

      Configure_IO
        (RX_Pin & TX_Pin,
         (Mode           => Mode_AF,
          AF             => GPIO_AF_USART2_7,
          Resistors      => Pull_Up,
          AF_Speed       => Speed_50MHz,
          AF_Output_Type => Push_Pull));

      Disable (Transceiver);

      Set_Baud_Rate    (Transceiver, Baudrate);
      Set_Mode         (Transceiver, Tx_Rx_Mode);
      Set_Stop_Bits    (Transceiver, Stopbits_1);
      Set_Word_Length  (Transceiver, Word_Length_8);
      Set_Parity       (Transceiver, No_Parity);
      Set_Flow_Control (Transceiver, No_Flow_Control);

      Enable (Transceiver);
   end Initialize;

 procedure Read_Blocking 
      (Data : out UInt9;
      Status: out UART_Status; 
      Timeout : Time_Span := Seconds (1)) is
      Start_Time : constant Time := Clock;
   begin
      loop 
         if Rx_Ready (Transceiver) then
            Receive (Transceiver, Data);
            Status := HAL.UART.Ok;
            exit;
         elsif (Clock - Start_Time) > Timeout then
            Status := HAL.UART.Err_Timeout;
            exit;
         end if;
      end loop;
   end Read_Blocking;

   procedure Put_Blocking 
      (Data : UInt9; 
      Status: out UART_Status;
      Timeout : Time_Span := Seconds (1)) is
      Start_Time : constant Time := Clock;
   begin
      loop 
         if Tx_Ready (Transceiver) then
            Transmit (Transceiver, Data);
            Status := HAL.UART.Ok;
            exit;
         elsif (Clock - Start_Time) > Timeout then
            Status := HAL.UART.Err_Timeout;
            exit;
         end if;
      end loop;
   end Put_Blocking;

   procedure Transmit_String (Data: String) is
   Status : UART_Status := Ok;
   begin
      for C of Data loop
         Put_Blocking (UInt9 (Character'Pos(C)), Status);
         exit when Status /= Ok;         
      end loop;
   end Transmit_String;

   function Receive_String 
      (Delimiter: Character := ASCII.NUL;
      Timeout : Time_Span := Seconds (1)) return UART_String is
   Data : UInt9;
   Str: UART_String := Null_Bounded_String;
   Status: UART_Status := Ok;
   begin
      loop
         Read_Blocking (Data, Status, Timeout);
         exit when Status /= Ok;
         exit when Character'Val (Data) = Delimiter;
         exit when Length (Str) >= B_Str.Max_Length;
         Append (Str, Character'Val (Data));
      end loop;
      return Str;
   end Receive_String;

   function Read16(Status : out UART_Status) return Int16 is
      MSB, LSB : UInt9;
      Raw      : UInt16;
      Result   : Int16;
   begin
      UART_USB.Read_Blocking (MSB, Status, Ada.Real_Time.Time_Span_Last);
      UART_USB.Read_Blocking (LSB, Status, Ada.Real_Time.Time_Span_Last);

      Raw := UInt16(MSB) * 256 + UInt16(LSB);

      if Raw >= 2**15 then
         Result := Int16(Integer(Raw) - 2**16); -- Convertir en Int16 signé
      else
         Result := Int16(Raw);
      end if;

      return Result;
   end Read16;

   procedure Write16(Data: Int16 ; Status: out UART_Status) is
      Raw : UInt16;
   begin
      if Data < 0 then
         Raw := UInt16(2**16 + Integer(Data));  -- décalage signé vers non signé
      else
         Raw := UInt16(Data);
      end if;

      UART_USB.Put_Blocking (UInt9(Raw / 256), Status); -- MSB
      UART_USB.Put_Blocking (UInt9(Raw mod 256), Status); -- LSB
   end Write16;

   procedure Write(Data : T; Format : Endianness; Status : out UART_Status) is 
   type Byte_Array is array (1 .. T'Size / 8) of UInt8;
   function To_Bytes is new Ada.Unchecked_Conversion (T, Byte_Array);
   Bytes : Byte_Array := To_Bytes (Data);
   begin
      if Format = BIG_ENDIAN then
         for Byte of reverse Bytes loop
            Put_Blocking (UInt9 (Byte), Status);
         end loop;
      else
         for Byte of Bytes loop
            Put_Blocking (UInt9 (Byte), Status);
         end loop;
      end if;
   end;

   protected body Controller is

      procedure Enable_Interrupt is
      begin
         Enable_Interrupts (Device.all, source => Received_Data_Not_Empty);
         Is_Data := False;
         Raw_Input := Null_Bounded_String;
      end Enable_Interrupt;

      procedure Disable_Interrupt is
      begin
         Disable_Interrupts (Device.all, source => Received_Data_Not_Empty);
         Is_Data := False;
      end Disable_Interrupt;

      function Has_Data return Boolean is
      begin
         return Is_Data;
      end Has_Data;

      function Get_Data return UART_String is
      begin
         return Raw_Input;
      end Get_Data;

      procedure Handle_Reception is 
      Received_Char : constant Character := Character'Val (Current_Input (Device.all));
      begin
         case Command_State is
            when WAITING =>
               if Received_Char = '<' then
                  Raw_Input := Null_Bounded_String;
                  Command_State := PARSING;
                  Last_Char_Time := Clock;
               end if;
            when PARSING =>
               if Received_Char = '>' then
                  -- Inform main program 
                  -- Raw_Input := Null_Bounded_String;
                  Command_State := WAITING;

                  -- Disable UART while command is not processed
                  loop
                     exit when not Status (Device.all, Read_Data_Register_Not_Empty);
                  end loop;

                  Disable_Interrupts (Device.all, Source => Received_Data_Not_Empty);
                  Is_Data := True;

               else
                  -- Max Buffer length or timeout reached 
                  if (Length (Raw_Input) + 1) >= Max_Length or
                     (Clock - Last_Char_Time) > To_Time_Span(5.0) then
                     Command_State := WAITING;
                     -- Throw error
                  else
                     Append (Raw_Input, Received_Char);
                     Last_Char_Time := Clock;
                  end if;
               end if;
         end case;
      end Handle_Reception;

      procedure IRQ_Handler is
      begin
         --  check for data arrival
         if Status (Device.all, Read_Data_Register_Not_Empty) and
            Interrupt_Enabled (Device.all, Received_Data_Not_Empty)
         then
            Handle_Reception;
            Clear_Status (Device.all, Read_Data_Register_Not_Empty);
         end if;
      end IRQ_Handler;

   end Controller;


end UART_USB;