def parseMatrix (lines : List String) : (Array (Array Nat)) :=
  let parseLine := λx => (x.toList.map (λx => x.toNat - '0'.toNat)).toArray
  let l := lines.map parseLine
  l.toArray

def size (m: Array (Array Nat)) : Nat × Nat :=
  (m.size, m[0].size)

def contains10 (matrix: Array (Array Nat)) : Bool :=
  matrix.any (λx => x.any (λy => y == 10))

def replacePos (matrix: Array (Array Nat)) (i j v:Nat) : Array (Array Nat) :=
  let newLine := matrix[i].set! j v
  matrix.set! i newLine


def neighboursInc (matrix: Array (Array Nat)) (i j:Nat) : Array (Array Nat) := do
  let mut m := matrix
  m := if i > 0 then replacePos m (i-1) j (m[i-1][j]+1) else m
  m := if i < 9  then replacePos m (i+1) j (m[i+1][j]+1) else m
  m := if j > 0 then replacePos m i (j-1) (m[i][j-1]+1) else m
  m := if j < 9 then replacePos m i (j+1) (m[i][j+1]+1) else m
  m := if i > 0 && j > 0 then replacePos m (i-1) (j-1) (m[i-1][j-1]+1) else m
  m := if i > 0 && j < 9 then replacePos m (i-1) (j+1) (m[i-1][j+1]+1) else m
  m := if i < 9 && j > 0 then replacePos m (i+1) (j-1) (m[i+1][j-1]+1) else m
  m := if i < 9 && j < 9 then replacePos m (i+1) (j+1) (m[i+1][j+1]+1) else m
  m

partial def iterate (matrix: Array (Array Nat)) (positions: Array (Nat × Nat)) (acc:Nat) : Nat × Array (Array Nat) := do
  let mut m := matrix
  let mut f := acc
  let mut npos := #[]
  for i in [:10] do
    for j in [:10] do
      let change := m[i][j] >= 10 && not (positions.contains (i,j))
      m := if change then neighboursInc m i j else m
      f := if change then f + 1 else f
      npos := if change then npos.push (i, j) else npos
  if npos.size > 0 then
    iterate m (positions ++ npos) f
  else (f, m)

def reset (matrix: Array (Array Nat)) : Array (Array Nat) := 
  matrix.map ( λx => x.map (λy => if y > 9 then 0 else y)  )

def advance (matrix: Array (Array Nat)) : Array (Array Nat) := 
  matrix.map ( λx => x.map (λy => if y <= 9 then y+1 else y)  )

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let mut matrix := parseMatrix input.toList

  let mut flashes := 0
  for i in [:100] do
    let m := advance matrix

    let (fl, m) := iterate m #[] 0
    matrix := reset m
    flashes := flashes + fl
  
  IO.println flashes
  return 0