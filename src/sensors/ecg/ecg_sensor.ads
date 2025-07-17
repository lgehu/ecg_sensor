with Commands_Interpreter; use Commands_Interpreter;
with PanTompkins;

with Virtual_Sensor;
with Virtual_ADC;
with Sensor_Handler; use Sensor_Handler;


package Ecg_Sensor is

   type Ecg_Sensor_Type is new Virtual_Sensor.Sensor_Type with null record;

   overriding procedure Initialize (This : in out Ecg_Sensor_Type);
   
   overriding procedure Start      (This : in out Ecg_Sensor_Type);
   
   overriding procedure Stop       (This : in out Ecg_Sensor_Type);

   overriding function Get_Version (This : in out Ecg_Sensor_Type) return String;

   overriding function Get_Name    (This : in out Ecg_Sensor_Type) return String; 

   overriding function Is_Triggered (This : in out Ecg_Sensor_Type) return Boolean;
   
   overriding procedure  Process_Sample 
   (This : in out Ecg_Sensor_Type; 
   Sample_In : Virtual_ADC.Sample ; 
   Sample_Out : out Virtual_ADC.Sample);

   package Amplitude_Coef is new Commands_Interpreter.Real_Accessor (T => PanTompkins.Amplitude_Treshold_Coef_Type,
               Key            => "AMPLITUDE_COEF",
               Default_Value  => 1.5,
               Action_Fn      => Sensor_Handler.Return_Arg'Access
            );

   package Peak_Distance is new Commands_Interpreter.Real_Accessor (T => PanTompkins.Positive_Float,
               Key            => "PEAK_DISTANCE",
               Default_Value  => 0.260,
               Action_Fn      => Sensor_Handler.Return_Arg'Access
            );

   package Window_Sec is new Commands_Interpreter.Real_Accessor (T => PanTompkins.Positive_Float,
               Key            => "WINDOW_SEC",
               Default_Value  => 0.150,
               Action_Fn      => Sensor_Handler.Return_Arg'Access
            );

   package Output_Stage is new Commands_Interpreter.Discrete_Accessor (T => PanTompkins.Stage,
               Key            => "OUTPUT_STAGE",
               Default_Value  => PanTompkins.Stage_Integrated,
               Action_Fn      => Sensor_Handler.Return_Arg'Access
            );

end Ecg_Sensor;