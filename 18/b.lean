inductive SnailNumber
| Regular : Int → SnailNumber
| Pair : SnailNumber → SnailNumber → SnailNumber
  deriving Repr, BEq

open SnailNumber

def toStringSnail : SnailNumber → String
| Regular n => n.toNat.toSuperscriptString
| Pair a b => "[" ++ toStringSnail a ++ "," ++ toStringSnail b ++ "]"

instance : ToString SnailNumber where
  toString := toStringSnail

instance : Inhabited SnailNumber where
  default := Regular 0

notation "~" a => Regular a
notation "[" a "," b "]" => Pair a b



def addLeft (v:Int) : SnailNumber → SnailNumber 
| Regular n => Regular $ n + v
| Pair a b => Pair (addLeft v a) b

def addRight (v:Int) : SnailNumber → SnailNumber 
| Regular n => Regular $ n + v
| Pair a b => Pair a (addRight v b)




partial def explodeH (x:SnailNumber) (level:Nat := 0) : (SnailNumber × Bool × Int × Int) :=
match x with
| Regular n => (Regular n, false, 0, 0)
| Pair a b => match (level, a, b) with
  | (4, Regular ar, Regular br) =>
      (Regular 0, true, ar, br)
  | (_, a, b) =>
      let (na, ea, la, ra) := explodeH a (level+1)
      if ea then
        (Pair na (addLeft ra b), true, la, 0)
      else
        let (nb, eb, lb, rb) := explodeH b (level+1)
        if eb then
          (Pair (addRight lb a) nb, true, 0, rb)
        else
          (Pair a b, false, 0, 0)


def explode (x:SnailNumber) : Option SnailNumber :=
  let (n, exp, _, _) := explodeH x 0
  if exp then some n else none

def splitH : SnailNumber → (SnailNumber × Bool)
| Regular n => if n >= 10
      then (Pair (Regular $ n / 2) (Regular $ (n+1) / 2) , true) 
      else (Regular n, false)
| Pair a b =>
    let (na, sa) := splitH a
    if sa then (Pair na b, true)
    else
      let (nb, sb) := splitH b
      (Pair a nb, sb)


def split (x:SnailNumber) : Option SnailNumber := 
  let (n, spl) := splitH x
  if spl then some n else none


partial def reduce (x:SnailNumber) :=
  match explode x with
  | some n => reduce n
  | none => match split x with
    | some n1 => reduce n1
    | none => x

--#eval reduce $ [[[[[~9,~8],~1],~2],~3],~4]
--#eval reduce $ [~7,[~6,[~5,[~4,[~3,~2]]]]]
--#eval reduce $ [[[[[~4,~3],~4],~4],[~7,[[~8,~4],~9]]],[~1,~1]]


def add (x y:SnailNumber) : SnailNumber :=
  reduce $ Pair x y

def substring (x:String) (st e:Nat) := (x.drop st).take (e-st)

partial def splitPair (x:String) : String × String := do
  let mut counter := 0
  for i in [1:x.length-1] do
    if counter == 0 && (x.get i) == ',' then
      return (substring x 1 i, substring x (i+1) (x.length-1))
    else if (x.get i) == '[' then
      counter := counter + 1
    else if (x.get i) == ']' then
      counter := counter - 1
    else
      pure ()
  return ("", "")


partial def parseSnail (x:String) : SnailNumber :=
if x.get 0 == '[' then
  let (right, left) := splitPair x
  Pair (parseSnail right) (parseSnail left)
else
  Regular $ x.toInt!

def magnitude : SnailNumber → Int 
| Regular n => n
| Pair a b => 3 * (magnitude a) + 2 * (magnitude b)

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  
  let lsn := input.map parseSnail

  let mut maxMag := 0

  for i in [:lsn.size] do
    for j in [:lsn.size] do
      if i == j then pure () else
        let m := magnitude $ add lsn[i] lsn[j]
        maxMag := max m maxMag
  
  IO.println $ maxMag
  return 1