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


def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let matrix := parseMatrix input.toList
  let (w, h) := size matrix
  let mut total := 0

  for i in [:w] do
    for j in [:h] do
      let neighbourhood := (neighbours matrix i j).map (λ(x, y, z) => z)
      total := if neighbourhood.all (·>matrix[i][j]) then
                    total + matrix[i][j] + 1
               else
                    total


  IO.println total
  return 0