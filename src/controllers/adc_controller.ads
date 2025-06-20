with Interfaces; use Interfaces;

package ADC_Controller is 

   procedure Initialize;

   function Read_Value_Blocking return Unsigned_16;

end ADC_Controller;