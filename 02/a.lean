inductive Command
| Forward
| Down
| Up


def parseCmd : String → Command
| "forward" => Command.Forward
| "down" => Command.Down
| "up" => Command.Up
| _ => Command.Forward

def parseLine (line: String) : Command × Int :=
match String.splitOn line with
| [cmd, num] => (parseCmd cmd, String.toInt! num)
| _ => (Command.Forward, 0)


def coordinatePair : Command × Int → Int × Int
| (Command.Forward, n) => (0, n)
| (Command.Down, n) => (n, 0)
| (Command.Up, n) => (-n, 0)


def sumPairs : List (Int × Int) → (Int × Int)
| [] => (0,0)
| (d, w) :: xs => let (d1, w1) := sumPairs xs
                  (d + d1, w + w1)

def main (args : List String) : IO UInt32 :=
do let input ← IO.FS.lines "input.txt"
   let command_list := input.toList.map parseLine
   let depth_pairs := command_list.map coordinatePair
   let (d, w) := sumPairs depth_pairs
   IO.print (d * w)
   IO.print "\n"
   return 0