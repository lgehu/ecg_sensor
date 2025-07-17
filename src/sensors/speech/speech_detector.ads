with Virtual_Sensor;
with Virtual_ADC;

package Speech_Detector is

   type Speech_Detector_Type is new Virtual_Sensor.Sensor_Type with null record;

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
   
end Speech_Detector;