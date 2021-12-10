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


partial def detectFirstMiss (s:String) (stack:Array Char) (n:Nat) : Option Char :=
  if n == s.length then none else 
  let c := s.get n 
  if isOpen c then 
    detectFirstMiss s (stack.push c) (n+1)
  else if matching stack[stack.size - 1] == c then 
    detectFirstMiss s (stack.pop) (n+1)
  else
    some c



def scoreLine (s:String) : Nat :=
  match detectFirstMiss s #[] 0  with
    | none => 0
    | some c => score c
  


def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let scores := input.toList.map scoreLine
  let total := scores.foldr (·+·) 0
  IO.println total
  return 0