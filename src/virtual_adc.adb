with AdaData;
with HAL;           use HAL;
with STM32.Timers;  use STM32.Timers;
with STM32.Device;  use STM32.Device;
with STM32.ADC;     use STM32.ADC;
with STM32.Board;   use STM32.Board;
with STM32.GPIO;    use STM32.GPIO;

package body Virtual_ADC is

   type Sample_Buffer_Type is array (Natural range <>) of Sample;

   Sample_Buffer : Sample_Buffer_Type (1 .. 100);
   Sample_Index  : Natural := 1;
   pragma Atomic (Sample_Index);

   Flash_Index   : Natural := 1;

   All_Regular_Conversions : constant Regular_Channel_Conversions :=
   (1 => (Channel => 0, Sample_Time => Sample_15_Cycles));

   Epoch : Time := Clock; -- time reference for timestamp 
   Channel_Source : Input_Channel_Type := CH_ADC;   

   Sampling : Boolean := false;

   procedure Initialize_Timer is
   begin
      -- Enable TIMER
      Enable_Clock (ADC_Timer);

      Configure
      (ADC_Timer,
         Prescaler     => UInt16'Last,
         Period        => UInt32'Last,
         Clock_Divisor => Div1,
         Counter_Mode  => Up);

      Enable_Interrupt (ADC_Timer, Timer_Update_Interrupt);
   end Initialize_Timer;

   procedure Initialize_Btn is
   begin
      Enable_Clock (Peripherals.User_Btn);
      Configure_IO (Peripherals.User_Btn, (Mode_In, Resistors => Pull_Down));
   end Initialize_Btn;

   procedure Initialize_ADC is
   begin
      Enable_Clock (ADC_Input);
      Configure_IO (ADC_Input, (Mode => Mode_Analog, Resistors => Floating));

      Enable_Clock(ADC_Converter);

      Configure_Common_Properties
      (Mode           => Independent,
         Prescalar      => PCLK2_Div_2,
         DMA_Mode       => Disabled,
         Sampling_Delay => Sampling_Delay_5_Cycles);

      Configure_Unit
      (ADC_Converter,
         Resolution => ADC_Resolution_12_Bits,
         Alignment  => Right_Aligned);

      Configure_Regular_Conversions
      (ADC_Converter,
         Continuous  => False,
         Trigger     => Software_Triggered,
         Enable_EOC  => True,
         Conversions => All_Regular_Conversions);

      Enable_Interrupts (ADC_Converter, Regular_Channel_Conversion_Complete);

      Sampling := False;
   end Initialize_ADC;

   procedure Initialize is
   begin
      Initialize_Timer;
      Initialize_ADC;
      Initialize_Btn;

      Set_Sample_Rate (100);
   end Initialize;

   procedure Set_Sample_Rate (Sample_Rate : Positive) is
   Clock_Freq : constant Float := Float (System_Clock_Frequencies.SYSCLK) / 2.0;
   ARR     : constant := 10_000.0;
   PSC     : Float := Clock_Freq / ((ARR + 1.0) * Float (Sample_Rate) - 1.0); 
   begin
      Configure_Prescaler (ADC_Timer, UInt16 (Float'Rounding (PSC)), Update);
      Set_Autoreload (ADC_Timer, UInt32 (ARR));
   end Set_Sample_Rate;

   procedure Start_Sampling (Channel : Input_Channel_Type) is
   begin
      case Channel_Source is
         when CH_FLASH => 
            Enable (ADC_Timer);
         when CH_ADC =>
            Enable (ADC_Timer);
            Enable (ADC_Converter);
         when CH_BTN =>
            Enable (ADC_Timer);
      end case;
      Sampling := True;
      Sample_Index := 1;
      Channel_Source := Channel;
      Flash_Index := 1;
      Epoch := Clock;
   end Start_Sampling;

   procedure Stop_Sampling is
   begin
      case Channel_Source is
         when CH_FLASH => 
            Disable (ADC_Timer);
         when CH_ADC =>
            Disable (ADC_Timer);
            Disable (ADC_Converter);
         when others => null;
      end case;
      Sampling := False;
   end Stop_Sampling;

   function Is_Sampling return Boolean is 
   begin
      return Sampling;
   end Is_Sampling;

   procedure Add_Sample (Value : IEEE_Float_32) is
   begin
      Sample_Buffer (Sample_Index) := (Value => Value, 
                                       Timestamp =>  Clock - Epoch, 
                                       Channel_Source => Channel_Source);
      Sample_Index := (Sample_Index mod Sample_Buffer'Length) + 1;
   end Add_Sample;

   function Pop_Sample return Sample is
   S : Sample;
   begin
      Disable_Interrupt (ADC_Timer, Timer_Update_Interrupt);
      if Has_Sample then
         S := Sample_Buffer (Sample_Index - 1);
         Sample_Index := Sample_Index - 1;
      else
         S := (0.0, Clock - Epoch, Channel_Source);
      end if;
      Enable_Interrupt (ADC_Timer, Timer_Update_Interrupt);
      return S; 
   end Pop_Sample;

   function Has_Sample return Boolean is
   Index : Natural := 0;
   begin
      return Sample_Index > 1;
   end Has_Sample;

   protected body Controller is
      
      procedure Timer_IRQ is
      begin
         if Status (ADC_Timer, Timer_Update_Indicated) then
            if Interrupt_Enabled (ADC_Timer, Timer_Update_Interrupt) then
               Clear_Pending_Interrupt (ADC_Timer, Timer_Update_Interrupt);
               
               case Channel_Source is
                  when CH_FLASH => 
                     Add_Sample (AdaData.Data (Flash_Index));
                     Flash_Index := (Flash_Index mod AdaData.Data_Size) + 1;
                  when CH_BTN   =>
                     if User_Btn.Set then
                        Add_Sample (0.0);
                     else
                        Add_Sample (1000.0);
                     end if;
                  when CH_ADC   =>
                     Start_Conversion (ADC_Converter);
                  when others => null;
               end case;
               
            end if;
         end if;
      end Timer_IRQ;

      procedure ADC_IRQ is
      Value : UInt16; 
      begin
         if Status (ADC_Converter, Regular_Channel_Conversion_Complete) then
            if Interrupt_Enabled (ADC_Converter, Regular_Channel_Conversion_Complete) then
               Clear_Interrupt_Pending (ADC_Converter, Regular_Channel_Conversion_Complete);
               Value := Conversion_Value (ADC_Converter);
               Add_Sample (IEEE_Float_32 (Value));
            end if;
         end if;
      end ADC_IRQ;

   end Controller;

end Virtual_ADC;