module Program

open System.IO
open Day12

[<EntryPoint>]
let main args =
  let actual = File.ReadAllLines "Day12input.txt" |> (Array.toList >> parseInput >> gen 5 5000000I >> sum)
  0
  //let expected = 1672
  //Assert.Equal (expected, actual)