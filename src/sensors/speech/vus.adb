with vus_functions;
use vus_functions;

package body VUS is

   function Compute_Energy (Frame : Float_Array) return Float is
   begin
      return Sum((for I in Frame'Range => Frame(I) * Frame(I))) / Float(Frame'Length);
   end;

   function Compute_ZCR (Frame : Float_Array) return Float is
      Count : Float := 0.0;
   begin
      for I in 0 .. Frame'Length - 2 loop
         Count := Count + 0.5 * abs(Float(Sign(Frame(I + 1)) - Sign(Frame(I))));
      end loop;
      return Count;
   end;

   procedure Initialize (State : in out VUS_State; Init_Signal : in Float_Array) is
      N : constant := Init_Signal'Length / Hop_Size;
      subtype Index is Natural range 0 .. N - 1;

      Energies : vus_functions.Float_Array (Index) := (others => 0.0);
      ZCRs     : vus_functions.Float_Array (Index) := (others => 0.0);
      Frame    : Float_Array;
   begin
      State.Window := Hamming_Window(Frame_Length);

      for I in Index loop
         declare
            Start : constant Natural := I * Hop_Size;
         begin
            for J in Frame_Range loop
               Frame(J) := Init_Signal(Start + J) * State.Window(J);
            end loop;
            Energies(I) := Compute_Energy(Frame);
            ZCRs(I)     := Compute_ZCR(Frame);
         end;
      end loop;

      State.E_Threshold   := Mean(Energies) / 2.0;
      State.ZCR_Threshold := 1.5 * Mean(ZCRs) - 0.3 * StdDev(ZCRs);
   end;

   procedure VUS_Compute_Frame (
      State  : in out VUS_State;
      Frame  : in     Float_Array;
      Result :    out VUS_Label
   ) is
      Windowed : Float_Array;
      E, Z     : Float;
   begin
      for I in Frame_Range loop
         Windowed(I) := Frame(I) * State.Window(I);
      end loop;

      E := Compute_Energy(Windowed);
      Z := Compute_ZCR(Windowed);

      if E > State.E_Threshold then
         Result := Voiced;
      elsif Z < State.ZCR_Threshold then
         Result := Silent;
      else
         Result := Unvoiced;
      end if;
   end;

end VUS;
