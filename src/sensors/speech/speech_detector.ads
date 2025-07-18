with Virtual_Sensor;
with Virtual_ADC;

with vus; use vus;

package Speech_Detector is

   type Speech_Detector_Type is new Virtual_Sensor.Sensor_Type with record
      State : VUS.VUS_State; 
      Current_Label : VUS.VUS_Label;
      Trigger_Label : VUS.VUS_Label := VUS.Voiced;
      Buffer : VUS.Float_Array := (others => 0.0);
   end record;

   overriding procedure Initialize (This : in out Speech_Detector_Type);
   
   overriding procedure Start      (This : in out Speech_Detector_Type);
   
   overriding procedure Stop       (This : in out Speech_Detector_Type);

   overriding function Get_Version (This : in out Speech_Detector_Type) return String;

   overriding function Get_Name    (This : in out Speech_Detector_Type) return String;

   overriding function Is_Triggered (This : in out Speech_Detector_Type) return Boolean;

   overriding procedure Process_Sample 
   (This : in out Speech_Detector_Type; 
   Sample_In : Virtual_ADC.Sample ; 
   Sample_Out : out Virtual_ADC.Sample);
   
   -- Set the label which will trigger hook
   procedure Set_Trigger_Label (This : in out Speech_Detector_Type ; Label : VUS.VUS_Label);

end Speech_Detector;