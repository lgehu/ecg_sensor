with PanTompkins; use PanTompkins;
with Ada.Numerics.Elementary_Functions;

package body PanTompkins is

   -- Constants
   Fs           : constant IEEE_Float_32 := 100.0;       -- Sampling rate (Hz)
   Win_Size     : constant Natural := 30;                -- Window size for integration (~300 ms)
   Min_Distance : constant Natural := Natural(Fs * 0.6); -- Minimum samples between peaks (200 ms)

   -- Buffers and state
   type Buffer is array (Natural range <>) of IEEE_Float_32;

   Raw_Buffer      : Buffer(0 .. 4) := (others => 0.0); -- Last 5 samples for filters
   Deriv_Buffer    : Buffer(0 .. 3) := (others => 0.0);
   Squared_Buffer  : Buffer(0 .. Win_Size - 1) := (others => 0.0);

   Sample_Index    : Natural := 0;
   Last_Peak_Sample: Natural := 0;
   Peak_Count      : Natural := 0;
   Total_RR        : IEEE_Float_32 := 0.0;

   Heart_Rate      : IEEE_Float_32 := 0.0;

   procedure Initialize is
   begin
      Raw_Buffer      := (others => 0.0);
      Deriv_Buffer    := (others => 0.0);
      Squared_Buffer  := (others => 0.0);
      Sample_Index    := 0;
      Last_Peak_Sample:= 0;
      Peak_Count      := 0;
      Total_RR        := 0.0;
      Heart_Rate      := 0.0;
   end Initialize;

   -- Basic IIR filters: simple passband approximation using LP then HP
   function Low_Pass(New_Sample : IEEE_Float_32) return IEEE_Float_32 is
      -- Moving average over 5 points
   begin
      return (Raw_Buffer(0) + 2.0 * Raw_Buffer(1) + 2.0 * Raw_Buffer(2) + 2.0 * Raw_Buffer(3) + Raw_Buffer(4)) / 8.0;
   end Low_Pass;

   function High_Pass(Input : IEEE_Float_32) return IEEE_Float_32 is
   begin
      return Input - Raw_Buffer(2);
   end High_Pass;

   function Derivative return IEEE_Float_32 is
   begin
      return (2.0 * Deriv_Buffer(3) + Deriv_Buffer(2) - Deriv_Buffer(1) - 2.0 * Deriv_Buffer(0)) / 8.0;
   end Derivative;

   function Process_Sample(Sample : IEEE_Float_32) return IEEE_Float_32 is
      Filtered     : IEEE_Float_32;
      D            : IEEE_Float_32;
      Squared      : IEEE_Float_32;
      Integrated   : IEEE_Float_32 := 0.0;
      Threshold    : IEEE_Float_32;
   begin
      -- Shift raw buffer
      Raw_Buffer := Raw_Buffer(1 .. 4) & Sample;

      -- Apply LP + HP
      Filtered := High_Pass(Low_Pass(Sample));

      -- Derivative buffer
      Deriv_Buffer := Deriv_Buffer(1 .. 3) & Filtered;

      -- Derivation
      D := Derivative;

      -- Mise au carré
      Squared := D * D;

      -- Fenêtre glissante
      Squared_Buffer := Squared_Buffer(1 .. Win_Size - 1) & Squared;

      for X of Squared_Buffer loop
         Integrated := Integrated + X;
      end loop;

      Integrated := Integrated / IEEE_Float_32(Win_Size);

      -- Détection de pic
      Threshold := Integrated * 1.5;

      if Squared > Threshold and then (Sample_Index - Last_Peak_Sample > Min_Distance) then
         if Last_Peak_Sample > 0 then
            declare
               RR : IEEE_Float_32 := IEEE_Float_32(Sample_Index - Last_Peak_Sample) / Fs;
            begin
               Total_RR := Total_RR + RR;
               Peak_Count := Peak_Count + 1;
               Heart_Rate := 60.0 / (Total_RR / IEEE_Float_32(Peak_Count));
            end;
         end if;
         Last_Peak_Sample := Sample_Index;
      end if;

      Sample_Index := Sample_Index + 1;

      return Integrated;
   end Process_Sample;

   function Get_Heart_Rate return IEEE_Float_32 is
   begin
      return Heart_Rate;
   end Get_Heart_Rate;

end PanTompkins;
