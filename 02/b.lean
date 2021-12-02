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



def computeDepth (aim:Int) : List (Command × Int) → (Int × Int)
| [] => (0,0)
| ((Command.Down, v) :: xs) => computeDepth (aim + v) xs
| ((Command.Up, v) :: xs) => computeDepth (aim - v) xs
| ((Command.Forward, v) :: xs) => 
      let (d1, w1) := computeDepth aim xs
      return (d1 + (aim * v), w1 + v)

def main (args : List String) : IO UInt32 :=
do let input ← IO.FS.lines "input.txt"
   let command_list := input.toList.map parseLine
   let (d, w) := computeDepth 0 command_list
   IO.print (d * w)
   IO.print "\n"
   return 0