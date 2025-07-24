with STM32.Timers;  use STM32.Timers;
with STM32.Device;  use STM32.Device;
with STM32.Board;   use STM32.Board;
with STM32.GPIO;    use STM32.GPIO;
with HAL; use HAL;

package body Sampler is

   protected body Controller is

      procedure Initialize is
      begin
         Enable_Clock (ADC_Timer);

         Configure
         (ADC_Timer,
            Prescaler     => UInt16'Last,
            Period        => UInt32'Last,
            Clock_Divisor => Div1,
            Counter_Mode  => Up);

         Enable_Interrupt (ADC_Timer, Timer_Update_Interrupt);
      end Initialize; 

      procedure Start_Sampling is 
      begin
         Enable (ADC_Timer);
         Sampling := True;
      end Start_Sampling;

      procedure Stop_Sampling is
      begin
         Disable (ADC_Timer);
         Sampling := False;
      end Stop_Sampling;

      procedure Set_Channel (Ch : access Channels.Channel'Class) is 
      begin
         Current_Channel := Ch;
      end Set_Channel;
      
      procedure Timer_IRQ is
      begin
         if Status (ADC_Timer, Timer_Update_Indicated) then
            if Interrupt_Enabled (ADC_Timer, Timer_Update_Interrupt) then
               Clear_Pending_Interrupt (ADC_Timer, Timer_Update_Interrupt);
               --Hook;
               if Current_Channel /= null then
                  Current_Channel.all.Read_Channel;
               end if;
            end if;  
         end if;
      end Timer_IRQ;

      procedure Set_Hook (Hook : in out Sampling_Hook) is 
      begin
         Current_Hook := Hook;
      end Set_Hook;

      procedure Set_Sample_Rate (Sample_Rate : Natural) is 
      Clock_Freq : constant Float := Float (System_Clock_Frequencies.SYSCLK) / 2.0;
      ARR     : constant := 10_000.0;
      PSC     : Float := Clock_Freq / ((ARR + 1.0) * Float (Sample_Rate) - 1.0); 
      begin
         Configure_Prescaler (ADC_Timer, UInt16 (Float'Rounding (PSC)), Update);
         Set_Autoreload (ADC_Timer, UInt32 (ARR));
      end Set_Sample_Rate;      

   end Controller;

end Sampler;