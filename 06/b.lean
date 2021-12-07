partial def group : List (Nat × Nat) → Array (Nat × Nat)
| [] => #[]
| [x] => #[x]
| ((x1,c1)::(x2,c2)::xs) => 
    if x1 == x2 then
      group $ (x1, c1+c2)::xs
    else
      #[(x1, c1)].append $ group ((x2, c2) :: xs)

def mergeGroups (l:Array (Nat × Nat)) : Array (Nat × Nat) :=
  let sorted := l.insertionSort (λ(n1,c1) (n2, c2) => n1 < n2 )
  group $ sorted.toList

partial def simulate (days : Nat) (xs: List Nat) : Nat :=
do
  let mut fishes := #[]
  for n in xs do
    fishes := fishes.push (n, 1)
  for i in [:days] do
    fishes := mergeGroups fishes
    let numberOfChildren := if fishes[0].fst == 0 then fishes[0].snd else 0
    fishes := fishes.map ( λ(x,c) => if x == 0 then (6,c) else (x-1, c))
    let fishes2 := fishes
    fishes := 
      if numberOfChildren > 0 then 
        --dbgTrace ("hello" ++ fishes2.toList.toString ) $ fun _ =>
        fishes.push (8, numberOfChildren)
      else
        fishes
  
  let counts := fishes.map (λ(_, c) => c)
  return counts.foldr (·+·) 0

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let data_list := input.toList.head!.splitOn ","
  let days_list := data_list.map (·.toNat!)
  let after := simulate 256 days_list
  IO.println $ after
  return 0