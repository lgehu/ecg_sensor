with Peripherals; use Peripherals;
with Channels; use Channels;

package Sampler is

   type Sampling_Hook is access procedure;

   type Channel_Access is access all Channel'Class;

   protected type Controller is 
      pragma Interrupt_Priority;

      procedure Initialize;

      procedure Start_Sampling;

      procedure Stop_Sampling;

      procedure Set_Sample_Rate (Sample_Rate : Natural);

      -- Set the current channel to read on
      -- Sampler is not in charge to open/close the channel.
      procedure Set_Channel (Ch : access Channels.Channel'Class);

      procedure Set_Hook (Hook : in out Sampling_Hook);

      procedure Timer_IRQ with
      Attach_Handler => ADC_Timer_Interrupt;

   private

      Sampling : Boolean;
      Current_Hook : Sampling_Hook := null;
      Current_Channel : access Channels.Channel'Class := null;

   end Controller;

   Sampler_Ctrl : Controller;

end Sampler;