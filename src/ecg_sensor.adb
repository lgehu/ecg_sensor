with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Bounded;
with Interfaces; use Interfaces;

with HAL; use HAL;
with HAL.UART; use HAL.UART;

with UART_USB;
with PanTompkins;
with AdaData;
with Commands_Interpreter;

procedure Ecg_Sensor is

   ECG_VERSION : constant String := "0.1";
   LF_CR : constant String := ASCII.LF & ASCII.CR;

   type Sampling_Mode is (Mode_Loop, Mode_Onetime);
   type Sensor_State_Type is (INIT, SAMPLING, PAUSED);

   procedure Log (Msg : String) renames UART_USB.Transmit_String;

   procedure Return_Arg (Arg : Commands_Interpreter.Argument; Valid : Boolean) is
   begin
      if Valid then
         Log (Arg.Key'Image & " = " & Arg.Value'Image & LF_CR);
      else
         Log ("Invalid parameter" & LF_CR);
      end if;
   end;

   procedure Change_State (Arg : Commands_Interpreter.Argument; Valid : Boolean) is
   begin
      if Valid then
         null;
      end if;
   end;

   procedure Print_Args (Input : Commands_Interpreter.Argument; Valid : Boolean) is
   Args : Commands_Interpreter.Arg_Array (1 .. Commands_Interpreter.Get_Arg_Count);
   begin
      Commands_Interpreter.Get_Args (Args);
      for I in Args'Range loop
         Log (Args (I).Key'Image & " = " & Args (I).Value'Image & LF_CR);
      end loop;
   end Print_Args;

   package Get_Args is new Commands_Interpreter.Action_Accessor (
               Key => "GET_ARGS", 
               Action_Fn => Print_Args'Access);

   package Sample_Rate is new Commands_Interpreter.Real_Accessor (T => PanTompkins.Sampling_Frequency_Type,
               Key            => "SAMPLE_RATE",
               Default_Value  => 100.0,
               Action_Fn      => Return_Arg'Access
            );

   package Sensor_State is new Commands_Interpreter.Discrete_Accessor (T => Sensor_State_Type,
               Key            => "SENSOR_STATE",
               Default_Value  => INIT,
               Action_Fn      => Change_State'Access
            );

   package Amplitude_Coef is new Commands_Interpreter.Real_Accessor (T => PanTompkins.Amplitude_Treshold_Coef_Type,
               Key            => "AMPLITUDE_COEF",
               Default_Value  => 1.5,
               Action_Fn      => Return_Arg'Access
            );

   package Pick_Distance is new Commands_Interpreter.Real_Accessor (T => PanTompkins.Positive_Float,
               Key            => "PICK_DISTANCE",
               Default_Value  => 0.3,
               Action_Fn      => Return_Arg'Access
            );

   package Window_Sec is new Commands_Interpreter.Real_Accessor (T => PanTompkins.Positive_Float,
               Key            => "WINDOW_SEC",
               Default_Value  => 0.4,
               Action_Fn      => Return_Arg'Access
            );

   package Output_Stage is new Commands_Interpreter.Discrete_Accessor (T => PanTompkins.Stage,
               Key            => "OUTPUT_STAGE",
               Default_Value  => PanTompkins.Stage_Integrated,
               Action_Fn      => Return_Arg'Access
            );

   procedure Initialize is begin
      UART_USB.Initialize (115_200);
      UART_USB.Transmit_String ("ECG_SENSOR v0.1" & LF_CR);

      Amplitude_Coef.Register;
      Sample_Rate.Register;
      Pick_Distance.Register;
      Window_Sec.Register;
      Output_Stage.Register;
      Sensor_State.Register;
      Get_Args.Register;

   end Initialize;

   Result : IEEE_Float_32;
   Status : UART_Status;
   Raw_Command :UART_USB.UART_String;
   Arg : Commands_Interpreter.Argument;

begin
   
   begin
      Initialize;

      loop
         case Sensor_State.Get_Value is
            when INIT =>
               
               loop
                  Raw_Command := UART_USB.Receive_String (ASCII.Semicolon, Time_Span_Last);
                  
                  begin
                     Arg := Commands_Interpreter.Parse (UART_USB.B_Str.To_String (Raw_Command));
                  exception
                     when C : Commands_Interpreter.Commands_Exception =>
                        Log (Exception_Message (C));
                  end;
                  
                  exit when Sensor_State.Get_Value = SAMPLING;
               end loop;
               
               PanTompkins.Initialize ((Sampling_Frequency => Sample_Rate.Get_Value,
                                        Amplitude_Treshold_Coef => Amplitude_Coef.Get_Value,
                                        Minimal_Pick_Distance_Sec => Pick_Distance.Get_Value,
                                        Window_Sec => Window_Sec.Get_Value,
                                        Output_Stage => Output_Stage.Get_Value));
            when SAMPLING =>
               for I in AdaData.Data'Range loop
                  -- Read commands
                  Result := PanTompkins.Process_Sample (AdaData.Data(I));
                  UART_USB.Transmit_String(UART_USB.Int16(IEEE_Float_32'Max(IEEE_Float_32'Min(Result * 1000.0, 2.0**15 - 1.0), -2.0**15))'Image);
               end loop;
            when others =>

               null; 

         end case;

      end loop;

   exception -- Unknows Errors (if UART is working..)
      when C : Constraint_Error =>
         Log (Exception_Message (C));
      when P : Program_Error => 
         Log (Exception_Message (P));
   end;
end Ecg_Sensor;
