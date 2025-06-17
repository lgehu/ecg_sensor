with STM32.Device;         use STM32.Device;
with STM32.GPIO;           use STM32.GPIO;
with STM32.USARTs;         use STM32.USARTs;
with STM32.Timers;         use STM32.Timers;

with Ada.Interrupts;       use Ada.Interrupts;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;

with LED_Controller;       use LED_Controller;
with UART_USB;             use UART_USB;

package Peripherals is
   
   -- UART GPIO
   TX_Pin : aliased GPIO_Point := PA2;
   RX_Pin : aliased GPIO_Point := PA3;
   Transceiver : USART renames USART_2;
   Transceiver_Interrupt_Id : constant Interrupt_ID := USART2_Interrupt;
   Transceiver_AF : constant STM32.GPIO_Alternate_Function := GPIO_AF_USART1_7;

   USBCOM : UART_USB.Controller (TX_Pin'Access, 
                              RX_Pin'Access, 
                              Transceiver'Access,
                              Transceiver_Interrupt_Id, 
                              5000, '<', '>');

   -- LED and TIMER
   LED_Pin : aliased GPIO_Point := PA5;
   --LED_Timer : aliased Timer := Timer_7;
   Timer_Interrupt : constant Interrupt_ID := TIM7_Interrupt;

   User_Btn : constant GPIO_Point := PC13;
   
   LED_Ctrl : LED_Controller.Controller (Timer_7'Access, Timer_Interrupt, LED_Pin'Access);

end Peripherals;