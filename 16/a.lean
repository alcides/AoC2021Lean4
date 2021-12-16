inductive Bin
| b0
| b1
deriving Repr, BEq, Ord, Inhabited


open Bin

inductive Packet
| Literal : Nat → Nat → Packet
| Container : Nat → Nat → List Packet  → Packet
deriving Repr

open Packet


partial def flat {α:Type} : List (List α) → List α
| [] => []
| []::xs => flat xs
| (x::xs)::ys => x::(flat $ xs::ys)

def h2b : Char → List Bin
| '0' => [b0, b0, b0, b0]
| '1' => [b0, b0, b0, b1]
| '2' => [b0, b0, b1, b0]
| '3' => [b0, b0, b1, b1]
| '4' => [b0, b1, b0, b0]
| '5' => [b0, b1, b0, b1]
| '6' => [b0, b1, b1, b0]
| '7' => [b0, b1, b1, b1]
| '8' => [b1, b0, b0, b0]
| '9' => [b1, b0, b0, b1]
| 'A' => [b1, b0, b1, b0]
| 'B' => [b1, b0, b1, b1]
| 'C' => [b1, b1, b0, b0]
| 'D' => [b1, b1, b0, b1]
| 'E' => [b1, b1, b1, b0]
| 'F' => [b1, b1, b1, b1]
| _ => []

def hexaToBinary (x:String) : List Bin :=
flat $ x.toList.map (h2b)


def bin2dec (x:List Bin) : Nat := do
  let mut exp : Nat := 0
  let mut total := 0
  for i in x.reverse do
      let v := match i with
                | b0 => 0
                | b1 => 1
      let e : Nat := exp
      let add : Nat := v * (2 ^ e)
      total := total + add
      exp := exp + 1
  total

partial def parseLiteral (bread:Nat) (acc:List Bin) : List Bin → (Nat × Nat)
| [] => (0, 0)
| b0::rs => ( bin2dec (acc ++ rs.take 4), bread+5)
| b1::rs => parseLiteral (bread+5) (acc ++ rs.take 4) (rs.drop 4)



partial def decodeMultiplePackets (f:List Bin → (Packet × List Bin)) : List Bin → List Packet
| [] => []
| xs =>
    let (p, ps) := f xs
    p :: decodeMultiplePackets f ps

unsafe def decodePackets (bin:List Bin) : (Packet ×  List Bin) := do
    let mut remaining := bin
    let version := bin2dec $ remaining.take 3
    remaining := remaining.drop 3
    let packetType := bin2dec $ remaining.take 3
    remaining := remaining.drop 3
    if packetType == 4 then do
      let (literalContent, bread) := parseLiteral 0 [] remaining
      (Literal version literalContent, remaining.drop bread)
    else do
      -- operator
      let lengthType := remaining.head!
      remaining := remaining.tail!
      match lengthType with
      | b0 => do
          let nbits := bin2dec $ remaining.take 15
          remaining := remaining.drop 15
          let v := (remaining.take nbits)
          remaining := remaining.drop nbits
          (Container version packetType $ decodeMultiplePackets decodePackets v, remaining)

      | b1 =>
          let npackets := bin2dec $ remaining.take 11
          remaining := remaining.drop 11
          let mut ps := []
          for i in [:npackets] do
              let (p, rest) := decodePackets remaining
              ps := ps ++ [p]
              remaining := rest
          (Container version packetType $ ps, remaining)


unsafe def decode (x:String) : Packet :=
  (decodePackets $ hexaToBinary x).fst

partial def totalVersions : Packet → Nat
| (Literal v _) => v
| (Container v _ xs) => v + (xs.map totalVersions).foldr (·+·) 0

unsafe def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let encoded := input[0]
  IO.println $ totalVersions $ decode encoded

  return 0

