import Std.Data.HashMap


instance : Hashable Char where
  hash d := hash d.toNat

open Std

def readDigit (w:String) :=
  if "abdcdefg".all w.contains then 8 else
  if "abdcdfg".all w.contains then 9 else
  if "abcefg".all w.contains then 0 else
  if "abdefg".all w.contains then 6 else
  if "acdeg".all w.contains then 2 else
  if "acdfg".all w.contains then 3 else
  if "abdfg".all w.contains then 5 else
  if "bcdf".all w.contains then 4 else
  if "acf".all w.contains then 7 else
  if "cf".all w.contains then 1 else
  10


def readNumber : List Nat -> Nat
| [] => 0
| (n::ns) => n * (10 ^ ns.length) + readNumber ns


def apply (h:HashMap Char Char) (str:String) :=
  str.map (h.find!)


def h1 := (HashMap.empty.insert 'a' 'b').insert 'b' 'c'


def countUnique : List Char → HashMap Char Nat
| [] => HashMap.empty
| (x::xs) => let h := countUnique xs
             if h.contains x then
                h.insert x ((h.find! x) + 1)
             else
                h.insert x 1



def infer (line:String) : HashMap Char Char := do
  -- Find a as the difference between seven and one
  let letters := line.splitOn " "
  let one := List.head! $ letters.filter (·.length == 2)
  let seven := List.head! $ letters.filter (·.length == 3)
  let four := List.head! $ letters.filter (·.length == 4)
  let difference := List.head! $ seven.toList.filter (λx => not $ one.contains x)
  let hist := (countUnique line.toList).erase ' '
  let mut table := HashMap.empty
  for (key, val) in hist.toList do
    table := if val == 8 && key == difference then table.insert key 'a' else
             if val == 6 then table.insert key 'b' else 
             if val == 8 && key != difference then table.insert key 'c' else
             if val == 7 && four.contains key then table.insert key 'd' else
             if val == 4 then table.insert key 'e' else 
             if val == 9 then table.insert key 'f' else 
             if val == 7 && (not $ four.contains key) then table.insert key 'g' else
             table
  table := table.insert ' ' ' '
  table


def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let mut count := 0
  for line in input.toList do
    let parts := line.splitOn " | "
    let input := parts.get! 0
    let output := parts.get! 1
    let table := infer input
    let outNumbers := (apply table output).splitOn " "
    let finalNumber := readNumber $ outNumbers.map readDigit
    count := count + finalNumber

  IO.println count
  return 0