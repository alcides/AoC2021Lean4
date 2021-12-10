def score : Char → Nat
| ')' => 3
| ']' => 57
| '}' => 1197
| '>' => 25137
| _ => 0

def isOpen : Char → Bool
| '(' => true
| '[' => true
| '{' => true
| '<' => true
| _ => false

def matching : Char → Char
| '(' => ')'
| '[' => ']'
| '{' => '}'
| '<' => '>'
| _ => 'k'


partial def findMissing (s:String) (stack:Array Char) (n:Nat) : Option (Array Char) :=
  if n == s.length then 
    some $ stack.reverse.map matching
  else 
    let c := s.get n 
    if isOpen c then 
      findMissing s (stack.push c) (n+1)
    else if matching stack[stack.size - 1] == c then 
      findMissing s (stack.pop) (n+1)
    else
      none

def val : Char → Nat
| ')' => 1
| ']' => 2
| '}' => 3
| '>' => 4
| _ => 0

partial def scoreString (a:Array Char) (pos acc: Nat) : Nat :=
if pos == a.size then acc else
scoreString a (pos+1) (acc * 5 + val a[pos] )

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let mut points := #[]
  for line in input.toList do
    points := match findMissing line #[] 0 with
      | none => points
      | some ps => do
                    points.push (scoreString ps 0 0)
  let finalScore := (points.insertionSort (· < ·))[ points.size / 2 ]
  IO.println finalScore
  return 0