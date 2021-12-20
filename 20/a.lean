def chr2nat : Char → Nat
| '#' => 1
| _ => 0

def bin2dec (x:List Nat) : Nat := do
  let mut exp : Nat := 0
  let mut total := 0
  for i in x.reverse do
      let e : Nat := exp
      let add : Nat := i * (2 ^ e)
      total := total + add
      exp := exp + 1
  total


def getNeighboursPos (im:Array (Array Nat)) (i j: Int) (bg:Nat) :=
  if i < 0 || i >= im.size then bg else
  if j < 0 || j >= im[0].size then bg else 
  let ri := i.toNat
  let rj := j.toNat
  im[ri][rj]

def getNeighbours (im:Array (Array Nat)) (i j: Int) (bg:Nat) :=
  [
    getNeighboursPos im (i-1) (j-1) bg,
    getNeighboursPos im (i-1) j bg,
    getNeighboursPos im (i-1) (j+1) bg,
    getNeighboursPos im i (j-1) bg,
    getNeighboursPos im i j bg,
    getNeighboursPos im i (j+1) bg,
    getNeighboursPos im (i+1) (j-1) bg,
    getNeighboursPos im (i+1) j bg,
    getNeighboursPos im (i+1) (j+1) bg
  ]

def enhance (tr:Array Nat) (im:Array (Array Nat)) (i j:Int) (bg:Nat) :=
  let base := getNeighbours im i j bg
  let num := bin2dec base
  tr[num]



def iterate (tr:Array Nat) (im:Array (Array Nat)) (bg:Nat) : Array (Array Nat) := do
  let mut m := #[]
  for fi in [:im.size + 2] do
    let mut line := #[]
    for fj in [:im[0].size + 2] do
      let px := enhance tr im ((Int.ofNat fi) - 1) ((Int.ofNat fj) - 1) bg
      line := line.push px
    m := m.push line
  m

def sum (arr: Array Nat) : Nat :=
  arr.foldr (λx y => x+y) 0

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  
  let tr := input[0].toList.toArray.map chr2nat
  let image := (input.toList.drop 2).toArray

  let nimage : Array (Array Nat) := image.map (·.toList.toArray.map chr2nat)


  let mut result := nimage
  let mut bg := 0
  for i in [:2] do
    result := iterate tr result bg
    bg := if bg == 0 then tr[0] else tr[511]
    --for line in result do
    --  IO.println $ line.toList.map (if · == 1 then '#' else '.')

  let totalLight : Nat := sum $ result.map sum

  IO.println $ totalLight

  return 0
