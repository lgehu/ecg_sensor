with Interfaces; use Interfaces;

package PanTompkins is

   procedure Initialize (Sampling_Frequency : IEEE_Float_32);

   -- Calcul the heart rate and return the data after the integration step. 
   function Process_Sample (Sample : IEEE_Float_32) return IEEE_Float_32;

   -- Depending on the sample frequency and heart reate of the ECG, you must
   -- skip Fs * 2 value before getting coherant value.
   function Get_Heart_Rate return IEEE_Float_32;

end PanTompkins;
