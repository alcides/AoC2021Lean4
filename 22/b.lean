structure Cube where
  xmi : Int
  xma : Int
  ymi : Int
  yma : Int
  zmi : Int
  zma : Int
  deriving BEq, Repr


def points (c:Cube) : Int :=
  (c.xma - c.xmi + 1) * (c.yma - c.ymi + 1) * (c.zma - c.zmi + 1)


def overlapsDim (c1 c2:Int×Int) : Bool :=
  not $ c1.snd < c2.fst || c2.snd < c1.fst

def overlaps (c1 c2:Cube) : Bool :=
  let ximi := (max c1.xmi c2.xmi)
  let xima := (min c1.xma c2.xma)
  let yimi := (max c1.ymi c2.ymi)
  let yima := (min c1.yma c2.yma)
  let zimi := (max c1.zmi c2.zmi)
  let zima := (min c1.zma c2.zma)
  ximi <= xima && yimi <= yima && zimi <= zima

def intersection (c1 c2:Cube) : Cube := do
  Cube.mk 
    (max c1.xmi c2.xmi)
    (min c1.xma c2.xma)
    (max c1.ymi c2.ymi)
    (min c1.yma c2.yma)
    (max c1.zmi c2.zmi)
    (min c1.zma c2.zma)

def parseInt (x:String) : Int :=
  if x[0] == '-' then
    - (Int.ofNat $ (x.drop 1).toNat!)
  else
    x.toNat!

def parseLine (x:String) : (Bool × Cube) :=
  let ps := x.splitOn " "
  let on := ps.head! == "on"
  let coordinates := (ps.get! 1).splitOn ","
  let ranges := coordinates.map (fun x => ((x.splitOn "=").get! 1).splitOn "..")

  (on, Cube.mk
      (parseInt $ (ranges.get! 0).get! 0)
      (parseInt $ (ranges.get! 0).get! 1)
      (parseInt $ (ranges.get! 1).get! 0)
      (parseInt $ (ranges.get! 1).get! 1)
      (parseInt $ (ranges.get! 2).get! 0)
      (parseInt $ (ranges.get! 2).get! 1)
      )

--#eval parseLine "off x=-40..-22,y=-38..-28,z=23..41"

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let actions := input.map parseLine

  let mut cubesOn := #[]

  for (on, cube) in actions do
      
    for (st, otherc) in cubesOn do
      if overlaps cube otherc then do
        cubesOn := cubesOn.push (not st, intersection cube otherc)
      else pure ()
    if on then
      cubesOn := cubesOn.push (on, cube)
    else pure ()
    --IO.println cubesOn

  let mut total := 0
  for (st, cube) in cubesOn do
    if st then
      total := total + points cube
    else 
      total := total - points cube

  IO.println $ total
  return 1