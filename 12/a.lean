def parseGraph (lines:List String) : List (String × String) :=
lines.map (λx => 
      let y := x.splitOn "-"
      (y.get! 0, y.get! 1)
)

def aToString (a: Array String) : String :=
  a.toList.toString

def isLarge (x:String) : Bool := not $ (x.takeWhile (·.isUpper)).isEmpty

partial def computeAllPaths (l: List (String × String)) (a: Array String) : Nat :=
  let current := a[a.size-1]
  if current == "end" then 
    dbgTrace (aToString a) $ fun _ => 1 
  else
    let possiblePaths := l.filter ( λ(o,d) => o == current) 
    let possiblePathsNoRepeats := possiblePaths.filter $ λ(o,d) => isLarge d || not (a.contains d)
    let pathsComputed : List Nat := possiblePathsNoRepeats.map $ λ(_,d) => computeAllPaths l (a.push d)
    pathsComputed.foldr (·+·) 0 


def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let pairs := parseGraph input.toList
  let revPairs := pairs.map $ λ(o,d) => (d, o)
  let bothPairs := pairs ++ revPairs

  IO.println $ computeAllPaths bothPairs #["start"]

  return 0