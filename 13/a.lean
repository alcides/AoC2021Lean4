inductive XY
| X
| Y

def parseFold (ln:String) : (XY × Nat) := do
  let parts := ln.splitOn " "
  let lastPart := parts.toArray[2]
  let parts2 := List.toArray $ lastPart.splitOn "="
  let xy := if parts2[0] == "x" then XY.X else XY.Y
  let n := parts2[1].toNat!
  (xy, n)

def parseFile (lines:Array String) : List (Nat × Nat) × List (XY × Nat) := do
  let base := lines.toList.takeWhile (λx => not $ x.isEmpty)
  let pairs := base.map $ λx => (x.splitOn ",").toArray
  let ps := pairs.map $ λline => (line[0].toNat!, line[1].toNat!)

  let otherbase := lines.toList.dropWhile (λx => not $ x.isEmpty)
  let xs := (otherbase.filter $ λx => not $ x.isEmpty).map parseFold
  (ps, xs)

def maxCoordinates : List (Nat × Nat) → Nat × Nat
| [] => (0, 0)
| ((x, y) :: ls) =>
    let (xm, ym) := maxCoordinates ls
    ( max xm (x+1), max ym (y+1) )

def bsum (x y:Nat) : Nat :=
if x == 1 || y == 1 then 1 else 0

def foldV (n:Nat) (page:Array (Array Nat)) : Array (Array Nat) := do
  let mut p := #[]
  for (i, e) in page.toList.enum do
    p := if i < n then p.push ( (Array.zip e page[n+n-i]).map $ λ(x,y) => bsum x y) else p
  p


def foldH (n:Nat) (line:Array Nat) : Array Nat := do
  let mut l := #[]
  for (i, e) in line.toList.enum do
    l := if i < n then l.push (bsum e line[n+n-i]) else l
  l

def foldPage (xs:XY) (n:Nat) (page:Array (Array Nat)) :=
  match xs with
  | XY.X => foldV n page
  | XY.Y => page.map (λline => foldH n line)


def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let (listPoints, folds) := parseFile input
  let (w,h) := maxCoordinates listPoints
  let mut page := List.toArray $ List.replicate w (List.toArray $ List.replicate h 0)
  for (x, y) in listPoints do
    page := page.set! x (page[x].set! y 1)
  for (xy, n) in folds do
    page := foldPage xy n page
    break

  let count := (page.map $ λli => li.foldr (·+·) 0 ).foldr (·+·) 0
  IO.println count
  return 1