with Peripherals; use Peripherals;

package Sampler is

   type Sampling_Hook is access procedure;

   protected type Controller is 
      pragma Interrupt_Priority;
   private

      procedure Initalize;

      procedure Start_Sampling;

      procedure Stop_Sampling;

      procedure Set_Sample_Rate (Sample_Rate : Natural);

      procedure Set_Hook (Hook : Sampling_Hook);

      procedure Timer_IRQ with
      Attach_Handler => ADC_Timer_Interrupt;

      Sampling : Boolean;
      Hook : Sampling_Hook := null;

   end Controller;

end Sampler;