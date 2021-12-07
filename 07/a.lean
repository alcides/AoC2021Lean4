
def computeMedian (arr: Array Nat) : Nat :=
  let middle := arr.size / 2
  arr[middle]

def sum (arr: Array Nat) : Nat :=
  arr.foldr (λx y => x+y) 0

def distance (x y: Nat) : Nat :=
if x > y then x - y else y - x

def distances (arr: Array Nat) (median: Nat) : Nat :=
  sum $ arr.map (λx => distance x median)

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let dataList := input.toList.head!.splitOn ","
  let dataListInt := dataList.map (·.toNat!)
  let sortedArray := dataListInt.toArray.insertionSort (λx y => x < y)
  let median := computeMedian sortedArray
  IO.println $ distances sortedArray median
  return 0