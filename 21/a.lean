def minimum (l:Array Nat) := l.foldr min l[0]

partial def play (positions:Array Nat) (it:Nat) (scores:Array Nat) (die:Nat) :=
if scores.any (·>=1000) then
    minimum scores * it * 3
else do
    let player := it % 2
    let totalDie := (die+1) % 100 + (die+2) % 100 + (die+3) % 100
    let npositions := positions.set! player $ (positions[player] + totalDie) % 10

    let mut nscores := scores.set! player $ scores[player] + if npositions[player] == 0 then 10 else npositions[player]
    --dbgTrace nscores.toList.toString $ fun _ =>
    play npositions (it+1) nscores (die+3)
   

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let positions := input.map (λx => (x.splitOn ": ").getLast!.toNat! )
  

  let r := play positions 0 #[0, 0] 0
  IO.println r

  return 1