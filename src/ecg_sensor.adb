with Ada.Exceptions; use Ada.Exceptions;
with Ada.Real_Time;
with Ada.Strings.Bounded;
with Interfaces; use Interfaces;

with HAL; use HAL;
with HAL.UART; use HAL.UART;

with UART_USB; use UART_USB;
with PanTompkins;
with AdaData;

procedure Ecg_Sensor is

   ECG_VERSION : constant String := "0.1";

   type Sampling_Mode is (Mode_Loop, Mode_Onetime);
   type Sensor_State is (INIT, SAMPLING, PAUSED);
   type Commands is (START, STOP, PAUSE, RESUME, VERSION, GET_PARAM, STATE);
   type Parameters_Type is (SAMPLE_MODE, SAMPLE_RATE, OUTPUT_STAGE, AMPLITUDE_COEF, PICK_DISTANCE, WINDOW_SEC);

   type Parameters_Array is array (Natural range <>) of IEEE_Float_32;

   Parameters : Parameters_Array(0 .. Parameters_Type'Pos (Parameters_Type'Last));  
   
   CMD_END : constant String := ASCII.LF & ASCII.CR;

   procedure Parse_Cmd(Command : UART_String; Arg : out UART_String; Value : out UART_String) is
      Equal_Pos : Natural := B_Str.Index (Command, "=", 1);
   begin
      if B_Str.Length (Command) < 3 or Equal_Pos < 2 then
         Arg := Command;
         Value := B_Str.To_Bounded_String("None");
         return;
      end if; 
      
      Arg := B_Str.Bounded_Slice(Command, 1, Equal_Pos - 1);
      Value := B_Str.Bounded_Slice(Command, Equal_Pos + 1, B_Str.Length (Command));

   end Parse_Cmd;

   function Get_Value(Param : Parameters_Type) return IEEE_Float_32 is
   begin
      return Parameters (Parameters_Type'Pos (Param));
   end Get_Value;

   procedure Set_Value(Param : Parameters_Type; Value : IEEE_Float_32) is 
   begin
      Parameters (Parameters_Type'Pos (Param)) := Value;
   end;

   procedure Set_Default_Parameters is
   begin
      Set_Value (SAMPLE_RATE, 100.0);
      Set_Value (OUTPUT_STAGE, IEEE_Float_32 (PanTompkins.Stage'Pos (PanTompkins.Stage_Integrated)));
      Set_Value (AMPLITUDE_COEF, 1.5);
      Set_Value (PICK_DISTANCE, 0.3);
      Set_Value (WINDOW_SEC, 0.4);
      Set_Value (SAMPLE_MODE, IEEE_Float_32 (Sampling_Mode'Pos (Mode_Loop)));
   end;

   procedure Print_Param is
   begin
      for I in Parameters_Type'Range loop
         UART_USB.Transmit_String (I'Image & "=" & Get_Value (I)'Image & CMD_END);
      end loop;
   end;

   function Read_Param return PanTompkins.Config is
      Command : UART_String;
      Arg     : UART_String;
      Value   : UART_String;
      Conf : PanTompkins.Config;
      Param : Parameters_Type;
   begin

      loop
         Command := UART_USB.Receive_String (ASCII.Semicolon, Ada.Real_Time.Time_Span_Last);
         Parse_Cmd (Command, Arg, Value);

         exit when B_Str.To_String (Arg) = "START";

         -- Action
         if B_Str.To_String(Value) = "None" then
            case Commands'Value (B_Str.To_String (Arg)) is
               when VERSION => UART_USB.Transmit_String ("ECG_SENSOR v" & ECG_VERSION & CMD_END);
               when GET_PARAM => Print_Param;
               when others => UART_USB.Transmit_String ("Unknown command" & CMD_END);
            end case;
         
         -- Parameter
         else 
            begin
               Param := Parameters_Type'Value (B_Str.To_String (Arg));
               Parameters (Parameters_Type'Pos (Param)) := IEEE_Float_32'Value (B_Str.To_String (Value));
               UART_USB.Transmit_String ("Ok" & CMD_END);
            exception
               when C : Constraint_Error =>
                  UART_USB.Transmit_String (Arg'Image & " is not a valid parameter: " & Exception_Message (C) & CMD_END);
            end;
         end if;
      end loop;
      
      return (Sampling_Frequency => Get_Value (SAMPLE_RATE), 
               Amplitude_Treshold_Coef => Get_Value (AMPLITUDE_COEF), 
               Minimal_Pick_Distance_Sec => Get_Value (PICK_DISTANCE), 
               Window_Sec => Get_Value (WINDOW_SEC), 
               Output_Stage => PanTompkins.Stage'Val (Natural (Get_Value (OUTPUT_STAGE))));

   end Read_Param;

   Result : IEEE_Float_32;
   Mode   : Sampling_Mode;
   Status : UART_Status;
   Config : PanTompkins.Config;
   ECG_State  : Sensor_State; 
   Param : PanTompkins.Config := (others => <>);

begin

   begin
      UART_USB.Initialize (115_200);
      UART_USB.Transmit_String ("ECG_SENSOR v0.1");

      Set_Default_Parameters;

      loop

         case ECG_State is
            when INIT =>
               Config := Read_Param;
               PanTompkins.Initialize (Config);
            when SAMPLING =>
               for I in AdaData.Data'Range loop
                  -- Read commands
                  Result := PanTompkins.Process_Sample (AdaData.Data(I));
                  UART_USB.Transmit_String(Int16(IEEE_Float_32'Max(IEEE_Float_32'Min(Result * 1000.0, 2.0**15 - 1.0), -2.0**15))'Image);
               end loop;
            when others =>

               null; 

         end case;

      end loop;

   exception -- Unknows Errors (if UART is working..)
      when C : Constraint_Error =>
         UART_USB.Transmit_String (Exception_Message (C));
      when P : Program_Error => 
         UART_USB.Transmit_String (Exception_Message (P));
      end;

end Ecg_Sensor;
