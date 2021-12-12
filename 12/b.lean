def parseGraph (lines:List String) : List (String × String) :=
lines.map (λx => 
      let y := x.splitOn "-"
      (y.get! 0, y.get! 1)
)

def aToString (a: Array String) : String :=
  a.toList.toString

def isSmall (x:String) : Bool := x == x.toLower
def isLarge (x:String) : Bool := not $ isSmall x

def countOf (x:String) (a:Array String) : Nat := Array.size $ a.filter (· == x)

partial def computeAllPaths (l: List (String × String)) (a: Array String) (visitedTwice: Bool) : Nat :=
  let current := a[a.size-1]
  if current == "end" then 
    dbgTrace (aToString a) $ fun _ => 1 
  else
    let possiblePaths := l.filter ( λ(o,d) => o == current) 

    let possiblePathsNoRepeats := possiblePaths.filter $ λ(o,d) => isLarge d || not (a.contains d) || (not visitedTwice && countOf d a < 2) 
    let pathsComputed : List Nat := possiblePathsNoRepeats.map $ λ(_,d) => computeAllPaths l (a.push d) ((a.contains d && isSmall d) || visitedTwice)
    pathsComputed.foldr (·+·) 0 


def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let pairs := parseGraph input.toList
  let revPairs := pairs.map $ λ(o,d) => (d, o)
  let bothPairs := (pairs ++ revPairs).filter (λ(o,d) => d != "start")

  IO.println $ computeAllPaths bothPairs #["start"] false

  return 0