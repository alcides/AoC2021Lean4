import Std.Data.HashMap

open Std

def minimum (l:Array Nat) := l.foldr min l[0]

def scoreOf : Nat → Nat
| 0 => 10
| i => i

structure Player where
  position:Nat
  score:Nat := 0
  deriving Hashable, BEq

def onePlay (p:Player) (d:Nat) :=
  let np := (p.position + d) % 10
  let ns := p.score + scoreOf np
  Player.mk np ns

partial def play (p1 p2:Player) (cache:HashMap (Player×Player) (Nat×Nat)) :=
  match cache[(p1,p2)] with
  | some v => (v, cache)
  | none =>
  if p2.score >= 21 then ((0, 1), cache.insert (p1, p2) (0, 1)) else do
  let mut mcache := cache
  let mut tr1 := 0
  let mut tr2 := 0
  for d1 in [1:4] do
      for d2 in [1:4] do
        for d3 in [1:4] do
          let ((r2, r1), rcache) := play p2 (onePlay p1 (d1+d2+d3)) mcache
          tr1 := tr1 + r1
          tr2 := tr2 + r2
          mcache := rcache
    ((tr1, tr2), mcache.insert (p1, p2) (tr1, tr2))

def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let positions := input.map (λx => (x.splitOn ": ").getLast!.toNat! )
  

  let ((r1, r2), _) := play (Player.mk positions[0] 0) (Player.mk positions[1] 0) HashMap.empty
  IO.println (max r1 r2)

  return 1