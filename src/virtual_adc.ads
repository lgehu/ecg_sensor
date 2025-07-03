with HAL;            use HAL;
with Interfaces;    use Interfaces;
with Ada.Real_Time; use Ada.Real_Time;

with Peripherals; use Peripherals;

package Virtual_ADC is

   type Input_Channel_Type is (CH_BTN, CH_FLASH, CH_ADC);

   type Sample is record 
      Value          : IEEE_Float_32;
      Timestamp      : Time_Span;
      Channel_Source : Input_Channel_Type;
   end record;

   Buffer_Size : constant := 50;

   procedure Initialize;

   procedure Set_Sample_Rate (Sample_Rate : Positive);

   procedure Start_Sampling (Channel : Input_Channel_Type);

   procedure Stop_Sampling;

   function Pop_Sample return Sample;

   function Has_Sample return Boolean;

   function Is_Sampling return Boolean;

   protected Controller is
      pragma Interrupt_Priority;
   private

      procedure Timer_IRQ with
      Attach_Handler => ADC_Timer_Interrupt;

      procedure ADC_IRQ with
      Attach_Handler => ADC_Converter_Interrupt;

   end Controller;


end Virtual_ADC;