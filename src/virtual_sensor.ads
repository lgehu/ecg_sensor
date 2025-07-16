with Virtual_ADC;

package Virtual_Sensor is
   
   type Sensor_Type is abstract tagged null record;

   procedure Initialize (This : in out Sensor_Type) is abstract;
   
   procedure Start      (This : in out Sensor_Type) is abstract;
   
   procedure Stop       (This : in out Sensor_Type) is abstract;

   function Get_Version (This : in out Sensor_Type) return String is abstract;

   function Get_Name    (This : in out Sensor_Type) return String is abstract;

   function Is_Triggered (This : in out Sensor_Type) return Boolean is abstract;

   function  Process_Sample 
   (This : in out Sensor_Type ; 
   Sample_In : Virtual_ADC.Sample ; 
   Sample_Out : out Virtual_ADC.Sample)
   return Boolean is abstract;

end Virtual_Sensor;