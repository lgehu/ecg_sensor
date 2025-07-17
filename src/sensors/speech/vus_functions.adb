with Ada.Numerics.Elementary_Functions;   use Ada.Numerics.Elementary_Functions;

package body vus_functions is
   
   function Mean(A : in Float_Array) return Float is
   begin
      return Sum(A) / Float(A'Length);
   end Mean;
   
   
   
   function Zeros(length : Positive) return Float_Array is
      Result : constant Float_Array(1 .. length) := (others => 0.0);
   begin
      return Result;
   end Zeros;
   
   
   
   function Sum(A : Float_Array) return Float is
      Total : Float := 0.0;
   begin
      for I in A'Range loop
         Total := Total + A(I);
      end loop;
      return Total;
   end Sum;


   
   function Diff(A : Float_Array) return Float_Array is
   subtype Diff_Index is Integer range A'First .. A'Last - 1;
   Result : Float_Array(Diff_Index);
begin
   for I in Result'Range loop
      Result(I) := A(I + 1) - A(I);
   end loop;
   return Result;
   end Diff;
   
   
   
   function Sign(X : Float) return Integer is
   begin
      if X < 0.0 then
         return -1;
      elsif X > 0.0 then
         return 1;
      else
         return 0;
      end if;
   end Sign;
   
   
   
   function Hamming_Window(N : Positive) return Float_Array is
      Result : Float_Array(1 .. N);
      Pi     : constant Float := 3.14159265;
   begin
      for i in Result'Range loop
         Result(i) := 0.54 - 0.46 * Cos(2.0 * Pi * Float(i - 1) / Float(N - 1));
      end loop;
      return Result;
   end Hamming_Window;
   


   function StdDev (A : Float_Array) return Float is
      M   : constant Float := Mean(A);
      Sum : Float := 0.0;
   begin
      for I of A loop
         Sum := Sum + (I - M) * (I - M);
      end loop;
      return Sqrt(Sum / Float(A'Length));
   end StdDev;
   
   
   
   function Interpolate_VUS(
   VUS         : Float_Array;
   Frame_Length : Positive;
   Hop_Size     : Positive;
   Signal_Length: Natural
) return Float_Array is

   Result : Float_Array(0 .. Signal_Length - 1);
   N      : constant Natural := VUS'Length;

   -- D�but de la trame i : i * Hop_Size
   -- Milieu approx : i * Hop_Size + Frame_Length / 2
   Prev_Pos : Float := 0.0;
   Next_Pos : Float := 0.0;
   Start_Idx, End_Idx : Natural;
   Alpha : Float;
begin
   for I in 0 .. N - 2 loop
      Prev_Pos := Float(I) * Float(Hop_Size) + Float(Frame_Length) / 2.0;
      Next_Pos := Float(I + 1) * Float(Hop_Size) + Float(Frame_Length) / 2.0;

      Start_Idx := Natural(Float'Min(Prev_Pos, Float(Signal_Length - 1)));
      End_Idx   := Natural(Float'Min(Next_Pos, Float(Signal_Length - 1)));

      for J in Start_Idx .. End_Idx loop
         Alpha := (Float(J) - Prev_Pos) / (Next_Pos - Prev_Pos);
         Result(J) := (1.0 - Alpha) * VUS(I) + Alpha * VUS(I + 1);
      end loop;
   end loop;

   -- �tendre les derniers points avec la derni�re valeur
   for J in Natural(Next_Pos) .. Signal_Length - 1 loop
      Result(J) := VUS(N - 1);
   end loop;

   return Result;
   end Interpolate_VUS;
   
   
   function VUS_From_Frame(Frame : Float_Array) return Float is
   Energy : Float := 0.0;
   ZCR    : Float := 0.0;
   Hamming : constant Float_Array := Hamming_Window(Frame'Length);
begin
   for I in Frame'Range loop
      Energy := Energy + (Frame(I) * Hamming(I))**2;
   end loop;
   Energy := Energy / Float(Frame'Length);

   for I in 0 .. Frame'Length - 2 loop
      ZCR := ZCR + 0.5 * abs(Float(Sign(Frame(I + 1)) - Sign(Frame(I))));
   end loop;

   if Energy > 0.01 then  -- Seuil fixe ou dynamique
      return 1.0;
   elsif ZCR < 0.2 then
      return 0.0;
   else
      return 0.5;
   end if;
end VUS_From_Frame;
   

end vus_functions;
