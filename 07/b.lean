def sum (arr: Array Nat) : Nat :=
  arr.foldr (λx y => x+y) 0

def computeAverage (arr: Array Nat) : Nat × Nat :=
  let dd := (sum arr) / arr.size
  (dd, dd+1)

def cost (n : Nat) : Nat := do
  let mut stepCost := 1
  let mut cost : Nat := 0
  for step in [1:n+1] do
      cost := cost + stepCost
      stepCost := stepCost + 1
  cost


def distance (x y: Nat) : Nat :=
  let steps := if x > y then x - y else y - x
  cost steps

def distances (arr: Array Nat) (middle: Nat) : Nat :=
  sum $ arr.map (λx => distance x middle)

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let dataList := input.toList.head!.splitOn ","
  let dataListInt := dataList.map (·.toNat!)
  let sortedArray := dataListInt.toArray.insertionSort (λx y => x < y)
  let (a1, a2) := computeAverage sortedArray
  let (d1, d2) := (distances sortedArray a1, distances sortedArray a2)
  let sol := min d1 d2
  IO.println $ sol
  return 0