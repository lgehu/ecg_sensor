with Virtual_ADC;

package Virtual_Sensor is
   
   type Sensor_Type;
   type Hook_Type is access procedure (This : in out Sensor_Type'Class ; S : Virtual_ADC.Sample);

   type Hook_Event is (ON_TRIGGER, ON_SAMPLE);
   
   type Sensor_Type is abstract tagged limited record
      Hook : Hook_Type := null;
      Event : Hook_Event;
   end record;

   procedure Initialize (This : in out Sensor_Type) is abstract;
   
   procedure Start      (This : in out Sensor_Type) is abstract;
   
   procedure Stop       (This : in out Sensor_Type) is abstract;

   function Get_Version (This : in out Sensor_Type) return String is abstract;

   function Get_Name    (This : in out Sensor_Type) return String is abstract;

   function Is_Triggered (This : in out Sensor_Type) return Boolean is abstract;

   procedure Set_Hook (This : in out Sensor_Type ; Hook : Hook_Type ; Event : Hook_Event);

   procedure  Handle_Sample 
   (This : in out Sensor_Type'Class; 
   Sample_In : Virtual_ADC.Sample ; 
   Sample_Out : out Virtual_ADC.Sample);

   -- remove Boolean 
   procedure  Process_Sample 
   (This : in out Sensor_Type ; 
   Sample_In : Virtual_ADC.Sample ; 
   Sample_Out : out Virtual_ADC.Sample) is abstract;

end Virtual_Sensor;