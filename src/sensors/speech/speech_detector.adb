package body Speech_Detector is

   SENSOR_VERSION : String := "0.1";
   SENSOR_NAME    : String := "Speech detector";

   overriding 
   procedure Initialize (This : in out Speech_Detector_Type) is
   Init_Signal : VUS.Float_Array := (others => 0.0); 
   begin
      VUS.Initialize (This.State, Init_Signal);
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
      VUS.VUS_Compute_Frame (This.State, This.Buffer, This.Current_Label);

      case This.Current_Label is
         when VUS.Voiced => 
            Sample_Out.Value := 1.0;
         when VUS.Silent =>
            Sample_Out.Value := 0.0;
         when VUS.Unvoiced =>
            Sample_Out.Value := 0.5;
      end case; 

   end Process_Sample;

   overriding
   function Is_Triggered (This : in out Speech_Detector_Type) return Boolean is
   begin
      if This.Current_Label = This.Trigger_Label then
         return True;
      else
         return False;
      end if;
   end Is_Triggered;

   procedure Set_Trigger_Label (This : in out Speech_Detector_Type ; Label : VUS.VUS_Label) is
   begin
      This.Trigger_Label := Label;
   end Set_Trigger_Label;
  
end Speech_Detector;