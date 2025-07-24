with Circular_Buffer;
with Peripherals; use Peripherals;
with UART_USB;

procedure Buffer_Test is

   package Float_Buffer is new Circular_Buffer (Element_Type => Float,
                                                Buffer_Size => 10);

   Buffer : Float_Buffer.Circular_Buffer;

begin

   Peripherals.USBCOM.Initialize (115_200);

   Buffer.Add (1.5);
   Buffer.Add (305.1);

   UART_USB.Transmit_String (USBCOM, "v1=" & Buffer.Pop'Image & ASCII.CR & ASCII.LF);
   UART_USB.Transmit_String (USBCOM, "v2=" & Buffer.Pop'Image & ASCII.CR & ASCII.LF);
   UART_USB.Transmit_String (USBCOM, "Count:" & Buffer.Count'Image & ASCII.CR & ASCII.LF);

end Buffer_Test;