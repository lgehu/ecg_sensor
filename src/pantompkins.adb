with Ada.Numerics.Elementary_Functions;

package body PanTompkins is

   -- Constants
   Default_Window_Sec : constant IEEE_Float_32 := 0.3;   -- 300 ms
   Default_Min_RR_Sec : constant IEEE_Float_32 := 0.4;   -- 400 ms
   RR_FIFO_Size       : constant := 10;                  -- Size of RR interval FIFO

   -- Sampling frequency
   Fs : IEEE_Float_32 := 100.0;
   Parameters : Config;

   -- Derived sizes
   Window_Size     : Natural;
   Min_Distance    : Natural;

   -- Buffers
   type Buffer is array (Natural range <>) of IEEE_Float_32;
   Raw_Buffer     : Buffer(0 .. 4) := (others => 0.0);
   Deriv_Buffer   : Buffer(0 .. 3) := (others => 0.0);
   type Buffer_Access is access all Buffer;
   Squared_Buffer : Buffer_Access;

   -- RR FIFO
   type RR_Array is array (0 .. RR_FIFO_Size - 1) of IEEE_Float_32;
   RR_Values      : RR_Array := (others => 0.0);
   RR_Index       : Natural := 0;
   RR_Count       : Natural := 0;

   -- State
   Sample_Index     : Natural := 0;
   Last_Peak_Sample : Natural := 0;
   Heart_Rate       : IEEE_Float_32 := 0.0;

   procedure Initialize (Param : Config := (others => <>)) is
   begin
      Parameters := Param;
      Fs := Param.Sampling_Frequency;
      Window_Size  := Natural(Fs * Param.Window_Sec);
      Min_Distance := Natural(Fs * Param.Minimal_Pick_Distance_Sec);
      Squared_Buffer := new Buffer(0 .. Window_Size - 1);
      Raw_Buffer := (others => 0.0);
      Deriv_Buffer := (others => 0.0);
      RR_Values := (others => 0.0);
      RR_Index := 0;
      RR_Count := 0;
      Sample_Index := 0;
      Last_Peak_Sample := 0;
      Heart_Rate := 0.0;
   end Initialize;

   function Low_Pass return IEEE_Float_32 is
   begin
      return (Raw_Buffer(0) + 2.0 * Raw_Buffer(1) + 2.0 * Raw_Buffer(2)
             + 2.0 * Raw_Buffer(3) + Raw_Buffer(4)) / 8.0;
   end Low_Pass;

   function High_Pass(Input : IEEE_Float_32) return IEEE_Float_32 is
   begin
      return Input - Raw_Buffer(2);
   end High_Pass;

   function Derivative return IEEE_Float_32 is
   begin
      return (2.0 * Deriv_Buffer(3) + Deriv_Buffer(2) - Deriv_Buffer(1) - 2.0 * Deriv_Buffer(0)) / 8.0;
   end Derivative;

   procedure Add_RR(RR : IEEE_Float_32) is
   begin
      RR_Values(RR_Index) := RR;
      RR_Index := (RR_Index + 1) mod RR_FIFO_Size;
      if RR_Count < RR_FIFO_Size then
         RR_Count := RR_Count + 1;
      end if;
   end Add_RR;

   function Average_RR return IEEE_Float_32 is
      Sum : IEEE_Float_32 := 0.0;
   begin
      for I in 0 .. RR_Count - 1 loop
         Sum := Sum + RR_Values(I);
      end loop;
      return Sum / IEEE_Float_32(RR_Count);
   end Average_RR;

   function Process_Sample(Sample : IEEE_Float_32) return IEEE_Float_32 is
      Filtered   : IEEE_Float_32;
      Deriv      : IEEE_Float_32;
      Squared    : IEEE_Float_32;
      Integrated : IEEE_Float_32 := 0.0;
      Threshold  : IEEE_Float_32;
   begin
      -- Shift buffers
      Raw_Buffer := Raw_Buffer(1 .. 4) & Sample;
      Filtered := High_Pass(Low_Pass);

      if Parameters.Output_Stage = Stage_Filtered then
         return Filtered;
      end if;

      Deriv_Buffer := Deriv_Buffer(1 .. 3) & Filtered;
      Deriv := Derivative;

      if Parameters.Output_Stage = Stage_Derivatived then
         return Deriv;
      end if;

      Squared := Deriv * Deriv;

      if Parameters.Output_Stage = Stage_Squared then
         return Squared;
      end if;

      for I in 0 .. Window_Size - 2 loop
         Squared_Buffer(I) := Squared_Buffer(I + 1);
      end loop;

      Squared_Buffer(Window_Size - 1) := Squared;

      for I of Squared_Buffer.all loop
         Integrated := Integrated + I;
      end loop;

      Integrated := Integrated / IEEE_Float_32(Squared_Buffer'Length);

      if Parameters.Output_Stage = Stage_Integrated then
         return Integrated;
      end if;

      Threshold := Integrated * Parameters.Amplitude_Treshold_Coef;

      if Squared > Threshold and then (Sample_Index - Last_Peak_Sample > Min_Distance) then
         if Last_Peak_Sample > 0 then
            declare
               RR : IEEE_Float_32 := IEEE_Float_32(Sample_Index - Last_Peak_Sample) / Fs;
            begin
               Add_RR(RR);
               Heart_Rate := 60.0 / Average_RR;
            end;
         end if;
         Last_Peak_Sample := Sample_Index;
      end if;

      Sample_Index := Sample_Index + 1;

      return Heart_Rate;
   end Process_Sample;

   function Get_Heart_Rate return IEEE_Float_32 is
   begin
      return Heart_Rate;
   end Get_Heart_Rate;

end PanTompkins;
