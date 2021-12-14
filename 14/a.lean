import Std.Data.HashMap


instance : Hashable Char where
  hash d := hash d.toNat

open Std

def parseTranfs (s:String) : String × Char := do
  let ps := List.toArray $ s.splitOn " -> "
  ( ps[0], ps[1].toList.head!)
      

def iterate (m:HashMap String Char) : List Char → List Char
| [] => []
| [x] => [x]
| (x::y::ys) => 
  let rs := iterate m (y::ys)
  match m.find? (x.toString ++ y.toString) with
  | none => x::rs
  | some c => x::c::rs

def countUnique (l:List Char) : HashMap Char Nat := do
  let mut h := HashMap.empty
  for x in l do
    h := match h.find? x with
        | none => h.insert x 1
        | some n => h.insert x (n+1)
  h

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let mut st := input[0].toList
  let transf := input.toList.drop 2

  let mut maps := HashMap.empty
  for line in transf do
      let (a, c) := parseTranfs line
      maps := maps.insert a c

  for i in [:10] do
    st := iterate maps st
  
  let h := countUnique st
  let vs := h.toList.map (λ(x,y) => y)
  let vma := vs.foldr max (vs.head!)
  let vmi := vs.foldr min (vs.head!)
  IO.println (vma - vmi)
  return 0