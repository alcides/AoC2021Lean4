import Std.Data.HashMap


instance : Hashable Char where
  hash d := hash d.toNat

open Std

def parseTranfs (s:String) : String × Char := do
  let ps := List.toArray $ s.splitOn " -> "
  ( ps[0], ps[1].toList.head!)
      
def listPairs (l:List Char) : List (Char × Char) :=
  let as := l.take (l.length-1)
  let bs := l.drop 1
  List.zip as bs

def uniqueWith {α:Type} [BEq α] [Hashable α] (h:HashMap α Nat) (x:α) (occ:Nat) :=
  match h.find? x with
        | none => h.insert x occ
        | some n => h.insert x (n+occ)

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let mut st := input[0].toList
  let transf := input.toList.drop 2

  let mut maps := HashMap.empty
  for line in transf do
      let (a, c) := parseTranfs line
      maps := maps.insert a c

  let mut expandable := HashMap.empty
  for (x,y) in listPairs st do
    expandable := uniqueWith expandable (x,y) 1

  let mut h : HashMap Char Nat := (HashMap.empty).insert (st.getLast!) 1

  for i in [:40] do
    let mut nextExpandable := HashMap.empty
    for ((x, y), c) in expandable.toList do
      nextExpandable := match maps.find? (x.toString ++ y.toString) with
          | none => nextExpandable
          | some n => 
              let s1 := uniqueWith nextExpandable (x, n) c
              let s2 := uniqueWith s1 (n, y) c
              s2
      h := match maps.find? (x.toString ++ y.toString) with
          | none => uniqueWith h x c
          | some n => h
      
    expandable := nextExpandable

  for ((x, y), c) in expandable.toList do
    h := uniqueWith h x c

  

  let vs := h.toList.map (λ(x,y) => y)
  let vma := vs.foldr max (vs.head!)
  let vmi := vs.foldr min (vs.head!)
  IO.println (vma - vmi)
  return 0