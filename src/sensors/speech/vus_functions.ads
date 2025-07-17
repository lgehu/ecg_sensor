with Ada;

package vus_functions is
   
   type Float_Array is array (Natural range <>) of Float;

   function StdDev (A : Float_Array) return Float;
   
   function Mean(A : in Float_Array) return Float;
   
   function Zeros(length : Positive) return Float_Array;
   
   function Sum(A : Float_Array) return Float;
   
   function Diff(A : Float_Array) return Float_Array;

   function Sign(X : Float) return Integer;
   
   function Hamming_Window(N : Positive) return Float_Array;
   
   function Interpolate_VUS(VUS : Float_Array; Frame_Length : Positive; Hop_Size : Positive; Signal_Length: Natural) return Float_Array;
   
   function VUS_From_Frame(Frame : Float_Array) return Float;

end vus_functions;
