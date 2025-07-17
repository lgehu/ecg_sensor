package body Speech_Detector is

   SENSOR_VERSION : String := "0.1";
   SENSOR_NAME    : String := "Speech detector";

   overriding 
   procedure Initialize (This : in out Speech_Detector_Type) is
   begin
      null;
   end Initialize;
   
   overriding
   procedure Start      (This : in out Speech_Detector_Type) is
   begin
      null;
   end Start;
   
   overriding 
   procedure Stop       (This : in out Speech_Detector_Type) is
   begin
      null;
   end Stop;

   overriding 
   function Get_Version (This : in out Speech_Detector_Type) return String is
   begin
      return SENSOR_VERSION;
   end Get_Version;

   overriding 
   function Get_Name    (This : in out Speech_Detector_Type) return String is
   begin
      return SENSOR_NAME;
   end Get_Name;

   overriding procedure  Process_Sample 
   (This : in out Speech_Detector_Type; 
   Sample_In : Virtual_ADC.Sample ; 
   Sample_Out : out Virtual_ADC.Sample) is
   begin
      null;
   end Process_Sample;

   overriding
   function Is_Triggered (This : in out Speech_Detector_Type) return Boolean is
   begin
      return False;
   end Is_Triggered;
  
end Speech_Detector;