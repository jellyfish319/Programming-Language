module F = Format

type t = NumV of int

let pp fmt (NumV n) = 
  F.fprintf fmt "(NumV %d)" n

