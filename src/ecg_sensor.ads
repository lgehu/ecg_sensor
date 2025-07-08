with Commands_Interpreter;
with Interfaces; use Interfaces;
with PanTompkins;
with AdaData;

with Ada.Interrupts.Names; use Ada.Interrupts.Names;

with STM32;                use STM32;
with STM32.Device;         use STM32.Device;
with STM32.GPIO;
with STM32.Timers;         use STM32.Timers;
with HAL;                  use HAL;

with Virtual_ADC;          use Virtual_ADC;

package Ecg_Sensor is

   subtype Sample_Rate_Type is Positive range 10 .. 1000;  

   procedure Initialize;

   procedure Update_Blocking;

   procedure Send_Command (Msg : String);

   private
      type Output_Format_Type is (OUT_ASCII, FLOAT32);

      procedure Start_Sampling (Input : Commands_Interpreter.Argument; Valid : Boolean);

      procedure Stop_Sampling (Input : Commands_Interpreter.Argument; Valid : Boolean);

      procedure Return_Arg (User_Input : Commands_Interpreter.Argument; Valid : Boolean);

      procedure Print_Args (User_Input : Commands_Interpreter.Argument; Valid : Boolean);

      procedure Reset_Sensor (User_Input : Commands_Interpreter.Argument; Valid : Boolean);

      procedure Send_Version (User_Input : Commands_Interpreter.Argument ; Valid : Boolean);

      procedure Init_Sampling (User_Input : Commands_Interpreter.Argument ; Valid : Boolean);

      package Sample_Rate is new Commands_Interpreter.Discrete_Accessor (T => Sample_Rate_Type,
                  Key            => "SAMPLE_RATE",
                  Default_Value  => AdaData.Sample_Rate,
                  Action_Fn      => Return_Arg'Access
               );

      package Amplitude_Coef is new Commands_Interpreter.Real_Accessor (T => PanTompkins.Amplitude_Treshold_Coef_Type,
                  Key            => "AMPLITUDE_COEF",
                  Default_Value  => 1.5,
                  Action_Fn      => Return_Arg'Access
               );

      package Peak_Distance is new Commands_Interpreter.Real_Accessor (T => PanTompkins.Positive_Float,
                  Key            => "PEAK_DISTANCE",
                  Default_Value  => 0.260,
                  Action_Fn      => Return_Arg'Access
               );

      package Window_Sec is new Commands_Interpreter.Real_Accessor (T => PanTompkins.Positive_Float,
                  Key            => "WINDOW_SEC",
                  Default_Value  => 0.150,
                  Action_Fn      => Return_Arg'Access
               );

      package Output_Stage is new Commands_Interpreter.Discrete_Accessor (T => PanTompkins.Stage,
                  Key            => "OUTPUT_STAGE",
                  Default_Value  => PanTompkins.Stage_Integrated,
                  Action_Fn      => Return_Arg'Access
               );

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

      package Enable_Trigger is new Commands_Interpreter.Discrete_Accessor (T => Boolean,
                  Key           => "ENABLE_TRIGGER",
                  Default_Value => FALSE,
                  Action_Fn     => Return_Arg'Access
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

      package Init_Cmd is new Commands_Interpreter.Action_Accessor (
                  Key            => "INIT", 
                  Action_Fn      =>  Init_Sampling'Access
               );

end Ecg_Sensor;