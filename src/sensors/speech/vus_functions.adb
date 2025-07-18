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


   
   function Diff(A : Float_Array) return Float_Array is
   subtype Diff_Index is Integer range A'First .. A'Last - 1;
   Result : Float_Array(Diff_Index);
begin
   for I in Result'Range loop
      Result(I) := A(I + 1) - A(I);
   end loop;
   return Result;
   end Diff;
   
end vus_functions;
