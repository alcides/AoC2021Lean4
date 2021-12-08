def main (args : List String) : IO UInt32 :=
do
  let input ← IO.FS.lines "input.txt"
  let dataList := input.toList.map (λx => x.splitOn " | ")
  let mut count := 0
  for line in dataList do
    let output := (line.get! 1).splitOn " "
    for word in output do
      count := if [2, 4, 3, 7].contains word.trim.length then
          count + 1
        else
          count
  IO.println count
  return 0