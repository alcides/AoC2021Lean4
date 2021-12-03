def parseBit : Char → Nat × Nat
| '0' => (1, 0)
| '1' => (0, 1)
| _ => (0, 0)


def parseLine (xs: String) : List (Nat × Nat) :=
  xs.toList.map parseBit


def SumPair ( a b:(Nat × Nat)) : (Nat × Nat)
:= (a.fst + b.fst, a.snd + b.snd)

def mergeLines (width:Nat) : List (List (Nat × Nat)) → List (Nat × Nat)
| [] => List.replicate width (0,0)
| (l :: ls) =>
  let rl := mergeLines width ls
  List.zipWith SumPair l rl

def minbits (w: Nat) : List (Nat × Nat) → Nat
| [] => 0
| (x::xs) => 
    let r := minbits (w-1) xs
    if x.fst < x.snd then
    r else r + 2 ^ w

def maxbits (w: Nat) : List (Nat × Nat) → Nat
| [] => 0
| (x::xs) => 
    let r := maxbits (w-1) xs
    if x.fst > x.snd then
    r else r + 2 ^ w


def main (args : List String) : IO UInt32 :=
do let input ← IO.FS.lines "input.txt"
   let command_list := input.toList.map parseLine
   let width := command_list.head!.length
   let merged := mergeLines width command_list
   let d := minbits (width-1) merged
   let w := maxbits (width-1) merged
   IO.print (d * w)
   IO.print "\n"
   return 0