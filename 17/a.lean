def nat2string (n:Nat) : String :=
  n.toSuperDigits.toString

def int2string (n:Int) : String := 
  if n < 0 then "-" ++ nat2string (-n).toNat
  else nat2string n.toNat

def abs (n:Int) : Nat := 
  if n < 0 then (-n).toNat else n.toNat

partial def downsqrt (n:Nat) (v:Nat := 1) : Nat :=
  if v*v > n then (v-1) else downsqrt n (v+1)
  

/-
instance : ToString Int where
  toString n := int2string n
-/

partial def simulate (x y vx vy hy:Int)
                     (target: ((Int × Int) × (Int × Int)))
                     : (Int × Bool) :=
  let ((xmi,xma), (ymi, yma)) := target
  let nx := x + vx 
  let nvx := vx + if vx > 0 then -1 else if vx < 0 then 1 else 0
  let ny := y + vy
  let nvy := vy - 1
  let nhy := max ny hy

  if xmi <= nx && nx <= xma &&
     ymi <= ny && ny <= yma then
     (nhy, true) 
  else if ny < ymi || nx > xma then
    (nhy, false)
  else
    simulate nx ny nvx nvy nhy target


unsafe def main (args : List String) : IO UInt32 :=
do
  /-
  let rawTarget := (
    (20, 30),
    (-10, -5)
  )-/
  let target := (
    (185, 221),
    (-122, -74)
  )

  let mut bestAltitude := 0
  for i in [0:target.fst.snd.toNat+1] do
    for j in [0:((-target.snd.fst) * 2).toNat] do
      let vx := Int.ofNat i
      let vy := Int.ofNat (j) + target.snd.fst
      let (altitude, inside) := simulate 0 0 vx vy 0 target
      bestAltitude :=
        if inside && altitude > bestAltitude then
          altitude
        else
          bestAltitude

  IO.println $ bestAltitude
  return 1