module Day9

open System.Text.RegularExpressions
open Xunit
open System.Collections.Generic
open System.Linq
open System.IO

let regex = Regex("(\d+) players; last marble is worth (\d+) points")
let parseInputData input =
  let m = regex.Match input
  let getValue i = m.Groups.[i + 1].Value |> int
  getValue 0, getValue 1

let playMarbles (players, lastMarble) =
  let ring = LinkedList [0]
  let clockwise, counterclockwise =
    let rec move f i m = if i = 0 then m else move f (i - 1) (f m)
    (move (fun (m : LinkedListNode<_>) -> Option.ofObj m.Next |> Option.defaultWith (fun () -> ring.First)),
     move (fun (m : LinkedListNode<_>) -> Option.ofObj m.Previous |> Option.defaultWith (fun () -> ring.Last)))
  let mutable currentPlayer = 0
  let mutable nextMarble = 1
  let mutable currentMarble = ring.First
  let mutable scores = Map.empty
  while nextMarble <= lastMarble do
    if nextMarble % 23 = 0 then
      let currentScore = (scores |> (Map.tryFind currentPlayer >> Option.defaultValue 0))
      let removeMarble = currentMarble |> counterclockwise 7
      let newScore = currentScore + nextMarble + removeMarble.Value
      currentMarble <- removeMarble |> clockwise 1
      scores <- scores |> Map.add currentPlayer newScore
      removeMarble |> ring.Remove 
    else
      currentMarble <- ring.AddAfter (currentMarble |> clockwise 1, nextMarble)
    currentPlayer <- (currentPlayer + 1) % players
    nextMarble <- nextMarble + 1
  let winner = if Map.isEmpty scores then (0, 0) else scores |> (Map.toSeq >> Seq.maxBy snd)
  winner, ring.AsEnumerable()

let placementTestData : obj [] [] = [|
  [| None;   0 ; [ 0                                                                                     ] |]
  [| Some 1; 1 ; [ 0; 1                                                                                  ] |]
  [| Some 2; 2 ; [ 0; 2; 1                                                                               ] |]
  [| Some 3; 3 ; [ 0; 2; 1; 3                                                                            ] |]
  [| Some 4; 4 ; [ 0; 4; 2; 1; 3                                                                         ] |]
  [| Some 5; 5 ; [ 0; 4; 2; 5; 1; 3                                                                      ] |]
  [| Some 6; 6 ; [ 0; 4; 2; 5; 1; 6; 3                                                                   ] |]
  [| Some 7; 7 ; [ 0; 4; 2; 5; 1; 6; 3; 7                                                                ] |]
  [| Some 8; 8 ; [ 0; 8; 4; 2; 5; 1; 6; 3; 7                                                             ] |]
  [| Some 9; 9 ; [ 0; 8; 4; 9; 2; 5; 1; 6; 3; 7                                                          ] |]
  [| Some 1; 10; [ 0; 8; 4; 9; 2; 10; 5; 1; 6; 3; 7                                                      ] |]
  [| Some 2; 11; [ 0; 8; 4; 9; 2; 10; 5; 11; 1; 6; 3; 7                                                  ] |]
  [| Some 3; 12; [ 0; 8; 4; 9; 2; 10; 5; 11; 1; 12; 6; 3; 7                                              ] |]
  [| Some 4; 13; [ 0; 8; 4; 9; 2; 10; 5; 11; 1; 12; 6; 13; 3; 7                                          ] |]
  [| Some 5; 14; [ 0; 8; 4; 9; 2; 10; 5; 11; 1; 12; 6; 13; 3; 14; 7                                      ] |]
  [| Some 6; 15; [ 0; 8; 4; 9; 2; 10; 5; 11; 1; 12; 6; 13; 3; 14; 7; 15                                  ] |]
  [| Some 7; 16; [ 0; 16; 8; 4; 9; 2; 10; 5; 11; 1; 12; 6; 13; 3; 14; 7; 15                              ] |]
  [| Some 8; 17; [ 0; 16; 8; 17; 4; 9; 2; 10; 5; 11; 1; 12; 6; 13; 3; 14; 7; 15                          ] |]
  [| Some 9; 18; [ 0; 16; 8; 17; 4; 18; 9; 2; 10; 5; 11; 1; 12; 6; 13; 3; 14; 7; 15                      ] |]
  [| Some 1; 19; [ 0; 16; 8; 17; 4; 18; 9; 19; 2; 10; 5; 11; 1; 12; 6; 13; 3; 14; 7; 15                  ] |]
  [| Some 2; 20; [ 0; 16; 8; 17; 4; 18; 9; 19; 2; 20; 10; 5; 11; 1; 12; 6; 13; 3; 14; 7; 15              ] |]
  [| Some 3; 21; [ 0; 16; 8; 17; 4; 18; 9; 19; 2; 20; 10; 21; 5; 11; 1; 12; 6; 13; 3; 14; 7; 15          ] |]
  [| Some 4; 22; [ 0; 16; 8; 17; 4; 18; 9; 19; 2; 20; 10; 21; 5; 22; 11; 1; 12; 6; 13; 3; 14; 7; 15      ] |]
  [| Some 5; 23; [ 0; 16; 8; 17; 4; 18; 19; 2; 20; 10; 21; 5; 22; 11; 1; 12; 6; 13; 3; 14; 7; 15         ] |]
  [| Some 6; 24; [ 0; 16; 8; 17; 4; 18; 19; 2; 24; 20; 10; 21; 5; 22; 11; 1; 12; 6; 13; 3; 14; 7; 15     ] |]
  [| Some 7; 25; [ 0; 16; 8; 17; 4; 18; 19; 2; 24; 20; 25; 10; 21; 5; 22; 11; 1; 12; 6; 13; 3; 14; 7; 15 ] |]
|]

let scoreTestData : obj [] [] = [|
  [| "9 players; last marble is worth 25 points";    32     |]
  [| "10 players; last marble is worth 1618 points"; 8317   |]
  [| "13 players; last marble is worth 7999 points"; 146373 |]
  [| "17 players; last marble is worth 1104 points"; 2764   |]
  [| "21 players; last marble is worth 6111 points"; 54718  |]
  [| "30 players; last marble is worth 5807 points"; 37305  |]
|]
     
[<Theory>]
[<MemberData("placementTestData")>]
let ``Marbles - Test Placement`` id lastMarble (input : int list) = 
  let (player, score), marbles = playMarbles (9, lastMarble)
  Assert.Equal<int list>(input, marbles |> Seq.toList)

[<Theory>]
[<MemberData("scoreTestData")>]
let ``Marbles Score - Test`` input expected = 
  let (player, score), marbles = input |> (parseInputData >> playMarbles)
  Assert.Equal (expected, score)

[<Fact>]
let ``Marbles Score - Actual`` () = 
  let (player, score), marbles = File.ReadAllText "Day9input.txt" |> (parseInputData >> playMarbles)
  let expected = 412117
  Assert.Equal (expected, score)