import Std.Data.HashMap

instance : Hashable Char where
  hash d := hash d.toNat

open Std

def parseMatrix (lines : List String) : (Array (Array Nat)) :=
  let parseLine := λx => (x.toList.map (λx => x.toNat - '0'.toNat)).toArray
  let l := lines.map parseLine
  l.toArray

def size (m: Array (Array Nat)) : Nat × Nat :=
  (m.size, m[0].size)

def updateCost (matrix: Array (Array Nat))
               (explored: HashMap (Nat × Nat) Nat)
               (x y d: Nat) := 
    match explored.find? (x, y) with
          | none => explored.insert (x, y) (d + matrix[x][y])
          | some c => explored.insert (x, y) (min c (d + matrix[x][y]))


def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "small.txt"
  let matrix := parseMatrix input.toList
  let (sx, sy) := size matrix
  let target := (sx-1, sy-1)

  let mut explored := HashMap.empty.insert (0,0) 0
  for i in [:sx*sy] do
    for ((x, y), d) in explored.toList do
      explored := if x < sx -1 then updateCost matrix explored (x+1) y d else explored
      explored := if y < sy -1 then updateCost matrix explored x (y+1) d else explored
      --explored := if x > 0 then updateCost matrix explored (x-1) y d else explored
      --explored := if y > 0 then updateCost matrix explored x (y-1) d else explored
    
    IO.println i
    IO.println explored.size
    IO.println "-"
    if explored.contains target then
      break
    else
      pure ()  
    
  match explored.getOp target with
  | none => pure ()
  | some n => IO.println n
  return 0