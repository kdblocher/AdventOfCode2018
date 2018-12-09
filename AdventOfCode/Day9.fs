module Day9

open System.Text.RegularExpressions
open Xunit


let regex = Regex("(\d+) players; last marble is worth (\d+) points")
let parseInputData input =
  let m = regex.Match input
  let getValue i = m.Groups.[i + 1].Value |> int
  getValue 0, getValue 1

let gameTestData : obj [] [] = [|
  [| "10 players; last marble is worth 1618 points"; 8317   |]
  [| "13 players; last marble is worth 7999 points"; 146373 |]
  [| "17 players; last marble is worth 1104 points"; 2764   |]
  [| "21 players; last marble is worth 6111 points"; 54718  |]
  [| "30 players; last marble is worth 5807 points"; 37305  |]
|]

let playMarbles (players, count) : int = 0

[<Theory>]
[<MemberData("gameTestData")>]
let ``Marbles - Test`` input expected = 
  let actual = input |> (parseInputData >> playMarbles)
  Assert.Equal (expected, actual)