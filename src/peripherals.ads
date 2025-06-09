with UART_USB;             use UART_USB;
with STM32.Device;         use STM32.Device;
with STM32.GPIO;           use STM32.GPIO;
with STM32.USARTs;         use STM32.USARTs;
with Ada.Interrupts;       use Ada.Interrupts;
with Ada.Interrupts.Names; use Ada.Interrupts.Names;

package Peripherals is
   
   TX_Pin : constant GPIO_Point := PA2;
   RX_Pin : constant GPIO_Point := PA3;
   
   Transceiver : USART renames USART_2;

   Transceiver_Interrupt_Id : constant Interrupt_ID := USART2_Interrupt;

   Transceiver_AF : constant STM32.GPIO_Alternate_Function := GPIO_AF_USART1_7;

   COM : UART_USB.Controller (Transceiver'Access, Transceiver_Interrupt_Id, 5000, '<', '>');

end Peripherals;