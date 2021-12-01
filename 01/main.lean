def countIncreases : List Int -> Int
| [] => 0
| [x] => 0
| (x :: y :: xs) => (if y > x then 1 else 0) + countIncreases (y :: xs)


def main (args : List String) : IO UInt32 :=
do let input ← IO.FS.lines "input.txt"
   IO.print $ (countIncreases (input.toList.map String.toInt!))
   IO.print "\n"
   return 0