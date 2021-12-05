structure Line where
  x1 : Nat
  y1 : Nat
  x2 : Nat
  y2 : Nat


def makeLine (x1 y1 x2 y2 : Nat) : Line :=
  let xmi := min x1 x2
  let xma := max x1 x2
  let ymi := min y1 y2
  let yma := max y1 y2
  {x1 := xmi, y1 := ymi, x2 := xma, y2 := yma}

def toStringLine (l:Line) :=
  toString (l.x1, l.x2, l.y1, l.y2)

instance : ToString Line where
  toString := toStringLine

def errorLine : Line := {x1 := 0, y1 := 0, x2 := 0, y2 := 0}

def parseLine (s:String) : Line :=
  match s.splitOn " -> " with
  | [p1, p2] => match (p1.splitOn ",", p2.splitOn ",") with
    | ([x1, y1], [x2, y2]) => makeLine x1.toNat! y1.toNat! x2.toNat! y2.toNat!
    | _ => errorLine
  | _ => errorLine

def inLine (l:Line) (x y: Nat) :=
  l.x1 <= x && x <= l.x2 && l.y1 <= y && y <= l.y2


def maxCoordinates : List Line → Nat × Nat
| [] => (0, 0)
| (l :: ls) =>
    let (xm, ym) := maxCoordinates ls
    ( max xm l.x2, max ym l.y2 )

def countOverlapPoint (ls: List Line) (x y: Nat) : Nat :=
  let insides := ls.filter (λl => inLine l x y)
  if insides.length > 1 then 1 else 0

partial def countOverlapsAux (ls: List Line) (ym x y: Nat) : Nat :=
countOverlapPoint ls x y + match (x, y) with
| (0, 0) => 0
| (x, 0) => countOverlapsAux ls ym (x-1) ym
| (x, y) => countOverlapsAux ls ym x (y-1)

partial def makePairs (ymax x y:Nat) : List (Nat × Nat) :=
(x, y) :: match (x, y) with
| (0, 0) => []
| (x, 0) => makePairs ymax (x-1) ymax
| (x, y) => makePairs ymax x (y-1)


def countOverlaps (ls: List Line) : Int :=
    let (xm, ym) := maxCoordinates ls
    let pairs := makePairs ym xm ym
    let counts := pairs.map (λ(x, y) => countOverlapPoint ls x y)
    return counts.foldr (λx y => x+ y) 0
    

def countPairs (ls: List Line) (xm ym:Nat) : IO Nat := do
  let mut count := 0
  for x in List.range xm do
    for y in List.range ym do
      count := count + countOverlapPoint ls x y     
  return count


def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let lines := input.toList.map parseLine
  let hvlines := lines.filter (λl => l.x1 == l.x2 || l.y1 == l.y2)
  --IO.println $ countOverlaps hvlines
  let (xm, ym) := maxCoordinates hvlines
  let total <- countPairs hvlines xm ym
  IO.println total
  return 0


