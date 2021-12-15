import Std.Data.HashMap

instance : Hashable Char where
  hash d := hash d.toNat

open Std

partial def upsqrt (n:Nat) (v:Nat := 1) : Nat :=
  if v*v >= n then v else upsqrt n (v+1)
  

def dist (x y:Nat) := upsqrt ( x*x + y*y )

def parseMatrix (lines : List String) : (Array (Array Nat)) :=
  let parseLine := λx => (x.toList.map (λx => x.toNat - '0'.toNat)).toArray
  let l := lines.map parseLine
  l.toArray

def size (m: Array (Array Nat)) : Nat × Nat :=
  (m.size, m[0].size)


partial def wrap9 (n:Nat) :=
  if n > 9 then wrap9 (n-9) else n


def getMatrix (matrix: Array (Array Nat)) (x y:Nat) : Nat :=
  let overx := x / matrix.size
  let overy := y / matrix[0].size
  let px := x % matrix.size
  let py := y % matrix[0].size
  wrap9 (matrix[px][py] + overx + overy)


partial def reachDestination 
    (sx sy: Nat)
    (m : Nat → Nat → Nat)
    (h: HashMap (Nat × Nat) Nat)
    (a: Array (Nat × Nat × Nat)) : IO Nat :=
  match h.find? (sx-1, sy-1) with
  | some n => n
  | none => do
    let (x,y,c) := a.back
    let mut neighbours := #[]
    --IO.println (x,y,c)
    neighbours := if x > 0 then neighbours.push (x-1,y) else neighbours
    neighbours := if y > 0 then neighbours.push (x,y-1) else neighbours
    neighbours := if x < sx-1 then neighbours.push (x+1,y) else neighbours
    neighbours := if y < sy-1 then neighbours.push (x,y+1) else neighbours
    let mut next := a.pop
    let mut nh := h
    for (nx, ny) in neighbours do

      let nc := (c + m nx ny)
      let (revisit, nv) := match nh.find? (nx, ny) with
                      | none => (true, nc)
                      | some d => (nc < d, min d nc)
      if revisit then do
        nh := nh.insert (nx, ny) nv
        next := next.push (nx, ny, nv)
      else
        pure ()

    reachDestination sx sy m nh (next.insertionSort (λ(_,_,c1) (_,_,c2) => c1 > c2))

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let matrix := parseMatrix input.toList
  let (sx, sy) := size matrix
  let (sxl, syl) := (5*sx, 5*sy)

  let mut h := HashMap.empty.insert (0,0) 0
  let sol ← reachDestination sxl syl (getMatrix matrix) h #[(0,0, 0)]
  IO.println $ sol
  return 0


