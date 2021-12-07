partial def simulate (n : Nat) (xs: List Nat) : Nat :=
do
  let mut fishes := xs.toArray
  for i in [:n] do
    let numberNewFishes := Array.size $ fishes.filter (· == 0)
    fishes := fishes.map ( λx => if x == 0 then 6 else x-1)
    for i in [:numberNewFishes] do
      fishes := fishes.push 8
  return fishes.size

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let data_list := input.toList.head!.splitOn ","
  let days_list := data_list.map (·.toNat!)
  let after := simulate 80 days_list
  IO.println $ after
  return 0