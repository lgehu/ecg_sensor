with Ecg_Sensor;
with Channel_Memory; 
with UART_USB; use UART_USB;
with Peripherals; use Peripherals;
with Sampler; use Sampler;
with Virtual_Adc; use Virtual_Adc;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces; use Interfaces;

procedure Pipeline_Demo is

   package Flash_Float is new Channel_Memory (Value_Type => IEEE_Float_32);

   Channel : aliased Flash_Float.Channel_Memory_Type (
         Buffer_Size => 10, 
         From_Addr => 16#8060000#,
         Length => 1000);
   
   Channel_Sample : Flash_Float.Channel_Gen.Sample;
   Sensor_Input : Virtual_Adc.Sample;
   Sensor_Output : Virtual_Adc.Sample;

   Sensor : Ecg_Sensor.Ecg_Sensor_Type;

begin

   begin
      USBCOM.Initialize (115_200);
      UART_USB.Transmit_String (USBCOM, "test");

      Sensor.Initialize;

      Channel.Open_Channel;
      
      Sampler_Ctrl.Initialize;
      Sampler_Ctrl.Set_Sample_Rate (200);
      Sampler_Ctrl.Set_Channel (Channel'Unchecked_Access);
      Sampler_Ctrl.Start_Sampling;

      loop
         if Channel.Has_Sample then
            -- Convert channel sample to sensor input type
            Channel_Sample := Channel.Pop_Sample;

            Sensor_Input   := (IEEE_Float_32 (Channel_Sample.Value), 
                              Channel_Sample.Timestamp,
                              CH_FLASH);

            Sensor.Process_Sample (Sensor_Input, Sensor_Output);

            UART_USB.Transmit_String (USBCOM, 
                                    Sensor_Output.Timestamp'Image & ";" & 
                                    Sensor_Output.Value'Image & ASCII.CR & ASCII.LF);
         end if;
      end loop;
   
   exception
      when E : Constraint_Error =>
         UART_USB.Transmit_String (USBCOM, Exception_Message (E));
      when E : Program_Error    => 
         UART_USB.Transmit_String (USBCOM, Exception_Message (E));
   end;

end Pipeline_Demo;