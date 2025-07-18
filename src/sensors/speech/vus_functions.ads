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
   
end vus_functions;
