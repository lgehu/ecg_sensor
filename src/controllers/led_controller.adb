with STM32.Board;   use STM32.Board;
with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;
with HAL; use HAL;

package body LED_Controller is

   protected body Controller is

      procedure Initialize is
      begin
         -- Configure LED
         Enable_Clock (GPIO.all);

         Configure_IO
         (GPIO.all,
            (Mode_Out,
            Resistors   => Floating,
            Output_Type => Push_Pull,
            Speed       => Speed_100MHz));

         -- Configure Timer
         Enable_Clock (Timer_Source.all);
         Reset (Timer_Source.all);

         Configure (Timer_Source.all, Prescaler => 13999, Period => 10000);

      end Initialize;

      procedure Start_Blinking is
      begin
         Enable_Interrupt (Timer_Source.all, Timer_Update_Interrupt);
         Enable (Timer_Source.all);
         STM32.Board.Turn_Off (GPIO.all);
      end Start_Blinking;

      procedure Stop_Blinking is
      begin
         Disable_Interrupt (Timer_Source.all, Timer_Update_Interrupt);
         Disable (Timer_Source.all);
         STM32.Board.Turn_Off (GPIO.all);
      end Stop_Blinking;

      procedure Set_Frequency (Freq : Float) is
      Clock_Freq : constant Float := 80_000_000.0;
      Divisor : constant Float := 18_000.0 * Freq;
      PSC : UInt16 := 0; 
      begin
         if Divisor > 0.0 then
            PSC := UInt16 (Clock_Freq / Divisor);
            Configure_Prescaler (Timer_Source.all, PSC, Update);
         end if;
      end Set_Frequency;

      procedure IRQ_Handler is
      begin
         if Status (Timer_Source.all, Timer_Update_Indicated) then
            if Interrupt_Enabled (Timer_Source.all, Timer_Update_Interrupt) then
               Clear_Pending_Interrupt (Timer_Source.all, Timer_Update_Interrupt);
               
               STM32.Board.Toggle (GPIO.all);
               Interrupt_Cnt := Interrupt_Cnt + 1;
               if Interrupt_Cnt > 6 then
                  Interrupt_Cnt := 1;
                  Stop_Blinking;
               end if;

            end if;
         end if;
      end IRQ_Handler;

   end Controller;


end LED_Controller;