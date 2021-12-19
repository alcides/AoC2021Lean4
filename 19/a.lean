def allRotations (a : Array (Int × Int × Int)) : Array (Array (Int × Int × Int)) := do
  let mut rots := #[]
  rots := rots.push a
  rots := rots.push $ a.map (λ(x,y,z) => (x, -z, y))
  rots := rots.push $ a.map (λ(x,y,z) => (z, y, -x))
  rots := rots.push $ a.map (λ(x,y,z) => (-y, x, z))
  rots

def allCombinations (a : Array (Int × Int × Int)) : Array (Array (Int × Int × Int)) := do
  let mut combs := #[]
  for a1 in allRotations a do
    for a2 in allRotations a1 do
      for a3 in allRotations a2 do
        for a4 in allRotations a3 do
          if combs.contains a4 then pure () else
            combs := combs.push a4
  combs

def allCombinationsFun : List ((Int × Int × Int) -> (Int × Int × Int)) :=
[
  λ(a, b, c) => (a, b, c),
  λ(a, b, c) => (b, c, a),
  λ(a, b, c) => (c, a, b),
  λ(a, b, c) => (c, b, -a),
  λ(a, b, c) => (b, a, -c),
  λ(a, b, c) => (a, c, -b),

  λ(a, b, c) => (a, -b, -c),
  λ(a, b, c) => (b, -c, -a),

  λ(a, b, c) => (c, -a, -b),
  λ(a, b, c) => (c, -b, a),
  λ(a, b, c) => (b, -a, c),
  λ(a, b, c) => (a, -c, b),

  λ(a, b, c) => (-a, b, -c),
  λ(a, b, c) => (-b, c, -a),
  λ(a, b, c) => (-c, a, -b),
  λ(a, b, c) => (-c, b, a),
  λ(a, b, c) => (-b, a, c),
  λ(a, b, c) => (-a, c, b),

  λ(a, b, c) => (-a, -b, c),
  λ(a, b, c) => (-b, -c, a),
  λ(a, b, c) => (-c, -a, b),
  λ(a, b, c) => (-c, -b, -a),
  λ(a, b, c) => (-b, -a, -c),
  λ(a, b, c) => (-a, -c, -b)
]

def allCombinationsAlt (a : Array (Int × Int × Int)) : Array (Array (Int × Int × Int)) := do
  let mut array := #[]
  for f in allCombinationsFun do
    array := array.push $ a.map f
  array

def sharedPoints (a b : Array (Int × Int × Int)) : (Nat × Array (Int × Int × Int)) := do
  let mut contained := 0
  let mut union := a
  for p in b do
    if a.contains p then
      contained := contained + 1
    else 
      union := union.push p
  (contained, union)

def findOverLap (a b : Array (Int × Int × Int)) : Option ((Int ×  Int × Int) × Array (Int × Int × Int)) := do
  let mut sol := none
  for (a1, a2, a3) in a do
    for f in allCombinationsFun do
      for i in [:b.size - 11] do
        let bp := b.map f
        let (b1, b2, b3) := bp[i]
        let d@(d1, d2, d3) := (a1-b1, a2-b2, a3-b3)
        let nb := bp.map $ λ(x, y, z) => (x+d1, y+d2, z+d3)
        let (contained, union) := sharedPoints a nb
        if contained < 12 then pure () else do
          sol := some $ (d, union)
          break
      if sol == none then pure () else break
    if sol == none then pure () else break
  sol
  




def merge (a b: Array (Int × Int × Int)) : Array (Int × Int × Int) :=
  match findOverLap a b with
  | none => dbgTrace "No overlap" $ fun _ => a
  | some (offset, b) => b


partial def mergeAll (acc: Array (Int × Int × Int)) : List (Array (Int × Int × Int)) → Array (Int × Int × Int)
| [] => acc
| (x :: ys) => match findOverLap acc x with
    | none => dbgTrace [ys.length + 1].toString $ fun _ => mergeAll acc (ys ++ [x])
    | some (offset, b) => mergeAll b ys
  


def scanTriple (x:String) : (Int × Int × Int)  :=
  let ls := ((x.splitOn ",").map (String.toInt!)).toArray
  (ls[0], ls[1], ls[2])

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"

  let mut scanners := #[]
  let mut currentScanner := #[]
  for line in input do
    if line.isEmpty then
        scanners := scanners.push currentScanner
    else if line.startsWith "--- scanner" then 
        currentScanner := #[]
    else do
          let triple := scanTriple line
          currentScanner := currentScanner.push triple
  scanners := scanners.push currentScanner

  
  let ground := mergeAll scanners[0] scanners.toList.tail!


  IO.println $ ground.size

  return 1