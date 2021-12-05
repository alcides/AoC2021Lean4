structure Line where
  x1 : Nat
  y1 : Nat
  x2 : Nat
  y2 : Nat


def makeLine (x1 y1 x2 y2 : Nat) : Line :=
  {x1 := x1, y1 := y1, x2 := x2, y2 := y2}

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



def isHorizontal (l:Line) : Bool := l.x1 == l.x2
def isVertical (l:Line) : Bool := l.y1 == l.y2
def isDiagonalRightDown (l:Line) : Bool := l.x1 < l.x2 && l.y1 < l.y2
def isDiagonalRightUp (l:Line) : Bool := l.x1 < l.x2 && l.y1 > l.y2
def isDiagonalLeftDown (l:Line) : Bool := l.x1 > l.x2 && l.y1 < l.y2
def isDiagonalLeftUp (l:Line) : Bool := l.x1 > l.x2 && l.y1 > l.y2


def inLine (l:Line) (x y: Nat) :=
if isHorizontal l then
  x == l.x1 && (min l.y1 l.y2 <= y && y <= max l.y1 l.y2)
else if isVertical l then
  (min l.x1 l.x2 <= x && x <= max l.x1 l.x2) && l.y1 == y
else if isDiagonalRightDown l then
  (x - l.x1) == (y - l.y1) && (l.x1 <= x && x <= l.x2) && (l.y1 <= y && y <= l.y2)
else if isDiagonalRightUp l then
  (x - l.x1) == (l.y1 - y) && (l.x1 <= x && x <= l.x2) && (l.y2 <= y && y <= l.y1)
else if isDiagonalLeftDown l then
  (l.x1 - x) == (y - l.y1) && (l.x2 <= x && x <= l.x1) && (l.y1 <= y && y <= l.y2)
else if isDiagonalLeftUp l then
  (l.x1 - x) == (l.y1 - y) && (l.x2 <= x && x <= l.x1) && (l.y2 <= y && y <= l.y1)
else
  false



def maxCoordinates : List Line → Nat × Nat
| [] => (0, 0)
| (l :: ls) =>
    let (xm, ym) := maxCoordinates ls
    ( max xm l.x2, max ym l.y2 )

def countOverlapPoint (ls: List Line) (x y: Nat) : Nat :=
  let insides := ls.filter (λl => inLine l x y)
  if insides.length > 1 then 1 else 0

def countPairs (ls: List Line) (xm ym:Nat) : IO Nat := do
  let mut count := 0
  for x in [0:xm+1] do
    for y in [0:ym+1] do
      count := count + countOverlapPoint ls x y
  return count


def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let lines := input.toList.map parseLine
  let (xm, ym) := maxCoordinates lines
  let total <- countPairs lines xm ym
  IO.println total
  return 0


