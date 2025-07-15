with Commands_Interpreter; use Commands_Interpreter;
with Virtual_ADC; use Virtual_ADC;
with Interfaces; use Interfaces;
with AdaData; use AdaData;
with Virtual_Sensor; use Virtual_Sensor;

package Sensor_Handler is
   
   type Output_Format_Type is (OUT_ASCII, FLOAT32);
   subtype Sample_Rate_Type is Positive range 10 .. 1000;  

   procedure Initialize (Sensor : in out Sensor_Type'Class);

   procedure Start_Sensor;

   procedure Send_Command (Msg : String);

   procedure Start_Sampling (Input : Commands_Interpreter.Argument; Valid : Boolean);

   procedure Stop_Sampling (Input : Commands_Interpreter.Argument; Valid : Boolean);

   procedure Return_Arg (User_Input : Commands_Interpreter.Argument; Valid : Boolean);

   procedure Print_Args (User_Input : Commands_Interpreter.Argument; Valid : Boolean);

   procedure Reset_Sensor (User_Input : Commands_Interpreter.Argument; Valid : Boolean);

   procedure Send_Version (User_Input : Commands_Interpreter.Argument ; Valid : Boolean);

  -- procedure Init_Sampling (User_Input : Commands_Interpreter.Argument ; Valid : Boolean);

   package Get_Args is new Commands_Interpreter.Action_Accessor (
               Key            => "GET_ARGS", 
               Action_Fn      => Print_Args'Access);

   package Output_Format is new Commands_Interpreter.Discrete_Accessor (T => Output_Format_Type,
               Key            => "OUTPUT_FORMAT", 
               Default_Value  => OUT_ASCII,
               Action_Fn      => Return_Arg'Access
            );

   package Input_Channel is new Commands_Interpreter.Discrete_Accessor (T => Virtual_ADC.Input_Channel_Type,
               Key           => "INPUT_CHANNEL",
               Default_Value => Virtual_ADC.CH_FLASH,
               Action_Fn     => Return_Arg'Access
            );

   package Sample_Rate is new Commands_Interpreter.Discrete_Accessor (T => Sample_Rate_Type,
      Key            => "SAMPLE_RATE",
      Default_Value  => AdaData.Sample_Rate,
      Action_Fn      => Return_Arg'Access
   );

   package Input_Gain is new Commands_Interpreter.Real_Accessor (T => IEEE_Float_32,
               Key            => "INPUT_GAIN",
               Default_Value  =>  1.0,
               Action_Fn      => Return_Arg'Access);
   
   package Start_Cmd is new Commands_Interpreter.Action_Accessor (
               Key            => "START", 
               Action_Fn      => Start_Sampling'Access);

   package Stop_Cmd is new Commands_Interpreter.Action_Accessor (
               Key            => "STOP", 
               Action_Fn      => Stop_Sampling'Access);

   package Reset_Cmd is new Commands_Interpreter.Action_Accessor (
               Key            => "RESET", 
               Action_Fn      => Reset_Sensor'Access
            );

   package Next_Cmd is new Commands_Interpreter.Discrete_Accessor ( T => Natural,
               Key            => "NEXT",
               Default_Value  => 0,
               Action_Fn      => Return_Arg'Access
            );

   package Version_Cmd is new Commands_Interpreter.Action_Accessor (
               Key            => "VERSION", 
               Action_Fn      => Send_Version'Access
            );

   --  package Init_Cmd is new Commands_Interpreter.Action_Accessor (
   --              Key            => "INIT", 
   --              Action_Fn      =>  Init_Sampling'Access
   --           );

end Sensor_Handler;