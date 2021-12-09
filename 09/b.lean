def parseMatrix (lines : List String) : (Array (Array Nat)) :=
  let parseLine := λx => (x.toList.map (λx => x.toNat - '0'.toNat)).toArray
  let l := lines.map parseLine
  l.toArray

def size (m: Array (Array Nat)) : Nat × Nat :=
  (m.size, m[0].size)


def neighbours (matrix: Array (Array Nat)) (i j:Nat) : Array (Nat × Nat × Nat) := do
  let (w, h) := size matrix
  let mut neighbours := #[]
  neighbours := if i > 0 then neighbours.push (i, j, matrix[i-1][j]) else neighbours
  neighbours := if j > 0 then neighbours.push (i, j, matrix[i][j-1]) else neighbours
  neighbours := if i < w-1 then neighbours.push (i, j, matrix[i+1][j]) else neighbours
  neighbours := if j < h-1 then neighbours.push (i, j, matrix[i][j+1]) else neighbours
  neighbours


def expandOnce (matrix: Array (Array Nat)) (ps: Array (Nat × Nat)) : Array (Nat × Nat) := do
  let (w, h) := size matrix
  let mut narray := ps
  for (i,j) in ps do
    narray := if i > 0 && matrix[i-1][j] < 9 && not (narray.contains (i-1, j)) then narray.push (i-1, j) else narray
    narray := if j > 0 && matrix[i][j-1] < 9 && not (narray.contains (i, j-1)) then narray.push (i, j-1) else narray
    narray := if i < w-1 && matrix[i+1][j] < 9 && not (narray.contains (i+1, j)) then narray.push (i+1, j) else narray
    narray := if j < h-1 && matrix[i][j+1] < 9 && not (narray.contains (i, j+1)) then narray.push (i, j+1) else narray
  narray


partial def expand (matrix: Array (Array Nat)) (ps: Array (Nat × Nat)) : Array (Nat × Nat) :=
  let narray := expandOnce matrix ps
  if ps.size == narray.size then ps else
  expand matrix narray

def computeBasin (matrix: Array (Array Nat)) (i j:Nat) : Nat := do
  Array.size $ expand matrix #[(i, j)]

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "small.txt"
  let matrix := parseMatrix input.toList
  let (w, h) := size matrix
  let mut basins := #[]

  for i in [:w] do
    for j in [:h] do
      let neighbourhood := (neighbours matrix i j).map (λ(x, y, z) => z)
      basins := if neighbourhood.all (·>matrix[i][j]) then
                    basins.push (computeBasin matrix i j)
               else
                    basins

  let threeLargest := (basins.insertionSort (· > ·)).toSubarray 0 3
  let mult := threeLargest.foldr (· * ·) 1
  IO.println $ mult
  return 0