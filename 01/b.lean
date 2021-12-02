open List

def countIncreases : List Int -> Int
| [] => 0
| [x] => 0
| (x :: y :: xs) => (if y > x then 1 else 0) + countIncreases (y :: xs)



def sum : List Int -> Int
| [] => 0
| (x :: xs) => x + sum xs

def movingWindow (n:Nat) (acc:List Int) : List Int -> List Int
| [] => []
| (x :: xs) => (sum (List.take n (x :: acc))) :: movingWindow n (x::acc) xs


def countMovingIncreases3 ( xs: List Int) : Int := countIncreases (movingWindow 3 (List.reverse $ List.take 2 xs) (List.drop 2 xs))

def main (args : List String) : IO UInt32 :=
do let input ← IO.FS.lines "input.txt"
   let intlist := input.toList.map String.toInt!
   IO.print $ (countMovingIncreases3 intlist)
   IO.print "\n"
   return 0