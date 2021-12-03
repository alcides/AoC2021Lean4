import Init.System.IO
open System

def string_to_nat_aux (w:Nat) : List Char → Nat
| [] => 0
| ('0'::xs) => string_to_nat_aux (w-1) xs
| (_::xs) => 2^w + string_to_nat_aux (w-1) xs


def string_to_nat (x:String) : Nat := 
  string_to_nat_aux (x.length - 1) x.toList


partial def bitfilter (f:Nat → Nat → Bool) (w:Nat) (lines: List String) : Nat :=
  if lines.length == 1
  then 
      string_to_nat lines.head!
  else 
    let hist := (lines.map (λx => x.get w))
    let ones := (hist.filter (λx => x == '1')).length
    let zeros := (hist.filter (λx => x == '0')).length
    let target := if f ones zeros then '1' else '0'
    let valid := lines.filter (λx => x.get w == target)
    bitfilter f (w+1) valid



unsafe def main (args : List String) : IO UInt32 :=
do let input ← IO.FS.lines "input.txt"
   let lines := input.toList
   let ogr := bitfilter (λx y => x >= y) 0 lines
   let csr := bitfilter (λx y => x < y) 0 lines
   IO.print (ogr * csr)
   IO.print "\n"
   return 0