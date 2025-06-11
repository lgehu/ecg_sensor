with Interfaces; use Interfaces;

package PanTompkins is

   type Stage is (Stage_Filtered, Stage_Derivatived, Stage_Squared, Stage_Integrated, Stage_HR);

   subtype Amplitude_Treshold_Coef_Type is IEEE_Float_32 range 0.0 .. 2.0;
   subtype Sampling_Frequency_Type is IEEE_Float_32 range 1.0 .. 1000.0;
   subtype Positive_Float is IEEE_Float_32 range 0.0 .. IEEE_Float_32'Last;

   type Config is record
      Sampling_Frequency : Sampling_Frequency_Type := 100.0;
      Amplitude_Treshold_Coef : Amplitude_Treshold_Coef_Type := 1.5;
      Minimal_Pick_Distance_Sec : Positive_Float := 0.4;
      Window_Sec : Positive_Float := 0.3;
      Output_Stage : Stage := Stage_Integrated;
   end record;

   procedure Initialize (Param : Config := (others => <>));

   -- Calcul the heart rate and return the data after the integration step. 
   function Process_Sample (Sample : IEEE_Float_32) return IEEE_Float_32;

   -- Depending on the sample frequency and heart rate (BPM) of the ECG, you must
   -- skip Fs * 2 value before getting coherant value. 
   function Get_Heart_Rate return IEEE_Float_32;

   -- Works only in stage HR
   function Is_Pick_Detected return Boolean;

end PanTompkins;
