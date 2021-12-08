import Std.Data.HashMap


open Std


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


def range (n m: Nat) :=
  let b := List.range ((m+1)-n) 
  b.map (· + n)


def getPoints (l:Line) : List (Nat × Nat) := 
  if isHorizontal l then
    let mi := min l.y1 l.y2
    let ma := max l.y1 l.y2
    let r := range mi ma
    r.map (l.x1,·)
  else if isVertical l then
    let mi := min l.x1 l.x2
    let ma := max l.x1 l.x2
    let r := range mi ma
    r.map (·, l.y1)
  else if isDiagonalRightDown l then
    let xs := range l.x1 l.x2
    let ys := range l.y1 l.y2
    (List.zip xs ys)
  else if isDiagonalRightUp l then
    let xs := range l.x1 l.x2
    let ys := List.reverse $ range l.y2 l.y1
    (List.zip xs ys)
  else if isDiagonalLeftDown l then
    let xs := List.reverse $ range l.x2 l.x1
    let ys := range l.y1 l.y2
    (List.zip xs ys)
  else if isDiagonalLeftUp l then
    let xs := range l.x2 l.x1
    let ys := range l.y2 l.y1
    (List.zip xs ys)
  else
    []

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let lines := input.toList.map parseLine
  let mut counter : HashMap (Nat × Nat) Nat := HashMap.empty

  for line in lines do
    for point in getPoints line do
      counter := match counter.find? point with
                  | none => counter.insert point 1
                  | some v => counter.insert point (v+1)

  let mut total := 0
  for (k, v) in counter.toList do
    total := if v > 1 then total + 1 else total
  IO.println total
  return 0


