with STM32.GPIO;           use STM32.GPIO;
with Ada.Interrupts;       use Ada.Interrupts;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;
with STM32.Timers;  use STM32.Timers;

package LED_Controller is

   -- Controller for blinking a LED 3 times at a given frequency.
   protected type Controller (Timer_Source : access Timer ; Timer_Interrupt : Interrupt_ID ; GPIO : access GPIO_Point) is

      procedure Initialize;

      procedure Start_Blinking;

      procedure Stop_Blinking;

      procedure Set_Frequency (Freq : Float);

   private
      procedure IRQ_Handler with Attach_Handler => Timer_Interrupt;

      Interrupt_Cnt : Natural := 1;

   end Controller;

end LED_Controller;