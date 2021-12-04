structure Cell where
  number: Int
  marked: Bool


def cellToString (c:Cell) : String :=
  (if c.marked then "+" else " ") ++ toString c.number

instance : ToString Cell where
  toString := cellToString

instance : Inhabited Cell where
  default := Cell.mk (-1) false

def Board := List (List Cell)
instance : ToString Board where
  toString := List.toString


partial def parseLine (line:String) : List Cell :=
  if line.length <= 0 then
    []
  else
    Cell.mk (line.take 2).trim.toInt! false :: parseLine (line.drop 3)


partial def parseBoard (lines:List String) : Board :=
  lines.map parseLine



partial def parseBoards: List String → List Board
| [] => []
| ( _ :: rest) =>
  let firstBoardRaw : List String := rest.take 5
  let otherBoardsRaw : List String := rest.drop 5
  parseBoard firstBoardRaw  :: parseBoards otherBoardsRaw


def playNumber (n: Int) (b: Board) : Board :=
  b.map ( λline => line.map ( λc => if c.number == n then Cell.mk n true else c  ) )


def sum : List Int → Int := List.foldl (λx y => x+y) 0

def sumUnmarked (b: Board) : Int :=
  sum (b.map (λline => sum (line.map (λc => if c.marked then 0 else c.number ))  ))

def allMarked (l:List Cell) := l.all (fun x => x.marked)

def anyMarked (l:List (List Cell)) := l.any allMarked

def rows (b:Board) := b
def columns (b:Board) := 
    (List.range 5).map (
      fun i => b.map ( λline => line.get! i )
    )


def isWinner (b:Board) : Bool :=
  anyMarked (rows b) || anyMarked (columns b)


def hasWinner : List Board → Option Board
| [] => Option.none
| (b :: bs) => if isWinner b then Option.some b else hasWinner bs


def findWinner (boards: List Board) : List Int → Int × Int
| [] => return (-1, -1)
| (n :: ns) => do
    let nboards := boards.map (playNumber n)
    match hasWinner nboards with
      | Option.none => findWinner (nboards) ns
      | (Option.some b) => ( sumUnmarked b , n)



def main (args : List String) : IO UInt32 :=
do let input ← IO.FS.lines "input.txt"
   let lines := input.toList
   match lines with
    | [] => return 0
    | (draws_raw :: boards_raw) => do
      let numbers := (draws_raw.splitOn ",").map String.toInt!
      let boards := parseBoards boards_raw
      let (totalBoard, finalNumber) := findWinner boards numbers
      IO.print (totalBoard * finalNumber)
      IO.print "\n"
      return 0