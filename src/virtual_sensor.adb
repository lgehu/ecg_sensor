package body Virtual_Sensor is

   procedure Set_Hook (This : in out Sensor_Type ; Hook : Hook_Type ; Event : Hook_Event) is
   begin
      This.Hook := Hook;
      This.Event := Event;
   end Set_Hook;

   procedure Handle_Sample 
   (This : in out Sensor_Type'Class ; 
   Sample_In : Virtual_ADC.Sample ; 
   Sample_Out : out Virtual_ADC.Sample) is
   begin
      This.Process_Sample (Sample_In, Sample_Out);

      case This.Event is
         when ON_SAMPLE =>
            This.Hook (This, Sample_Out);
         when ON_TRIGGER =>
            if This.Is_Triggered then
               This.Hook (This, Sample_Out);
            end if;
      end case;
   end Handle_Sample;

end Virtual_Sensor;