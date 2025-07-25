with HAL;           use HAL;
with HAL.UART;      use HAL.UART;
with STM32.Board;   use STM32.Board;
with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;
with STM32.Timers;  use STM32.Timers;
with Ada.Interrupts;       use Ada.Interrupts;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;

procedure Timer_Test is
Prescaler : constant UInt16 := UInt16 (((System_Clock_Frequencies.SYSCLK / 2) / 6000) - 1);
Period : constant := 6000 - 1;  

procedure Timer6_IRQ_Handler with
   Attach_Handler => STM32.Interrupts.Names.TIM6_DAC_Interrupt;

procedure Timer6_IRQ_Handler is
begin
   if Status (Timer_6, Timer_Update_Indicated) then
      if Interrupt_Enabled (Timer_6, Timer_Update_Interrupt) then
         -- Ici, ton code à chaque interruption (ex: toggle LED)
         Clear_Pending_Interrupt (Timer_6, Timer_Update_Interrupt);
      end if;
   end if;
end Timer6_IRQ_Handler;

begin
   Enable_Clock (Timer_6);

   Configure
     (Timer_6,
      Prescaler     => Prescaler,
      Period        => UInt32 (Period),
      Clock_Divisor => Div1,
      Counter_Mode  => Up);

   Enable_Interrupt (Timer_6, Timer_Update_Interrupt);

   Enable (Timer_6);

   -- Le timer tourne et exécute Timer6_IRQ_Handler toutes les secondes


end Timer_Test;