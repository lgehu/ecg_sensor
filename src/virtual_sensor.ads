with Virtual_ADC;

package Virtual_Sensor is
   
   type Sensor_Type;
   type Hook_Type is access procedure (This : in out Sensor_Type);
   
   type Sensor_Type is abstract tagged limited record
      Hook : Hook_Type := null;
   end record;

   procedure Initialize (This : in out Sensor_Type) is abstract;
   
   procedure Start      (This : in out Sensor_Type) is abstract;
   
   procedure Stop       (This : in out Sensor_Type) is abstract;

   function Get_Version (This : in out Sensor_Type) return String is abstract;

   function Get_Name    (This : in out Sensor_Type) return String is abstract;

   function Is_Triggered (This : in out Sensor_Type) return Boolean is abstract;

   procedure Set_Hook (This : in out Sensor_Type ; Hook : Hook_Type);

   -- remove Boolean 
   function  Process_Sample 
   (This : in out Sensor_Type ; 
   Sample_In : Virtual_ADC.Sample ; 
   Sample_Out : out Virtual_ADC.Sample)
   return Boolean is abstract;

end Virtual_Sensor;