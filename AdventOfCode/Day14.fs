module Day14

open System.Collections.Generic
open Helpers.LinkedList
open Xunit
open System.IO

let makeRecipes players lastRecipe =
  let scoredRecipes = 10
  let ring = LinkedList players
  let value (n : LinkedListNode<_>) = n.Value
  let players =
    let nodeAt i _ =
      let n = ring.First
      clockwise ring i n
    players |> Array.mapi nodeAt
  seq { 1 .. lastRecipe + scoredRecipes } |> Seq.iter (fun _ ->
    let getDigits i = seq { if i < 10 then yield i else yield i / 10; yield i % 10 }
    players |> (Seq.sumBy value >> getDigits >> Seq.iter (ring.AddLast >> ignore))
    players |> (Array.iteri (fun i r -> players.[i] <- clockwise ring (value r + 1) r)))
  let rec getScore i (n : LinkedListNode<_>) =
    if i = scoredRecipes then ""
    else n.Value.ToString() + getScore (i + 1) n.Next
  getScore 0 (ring.First |> clockwise ring lastRecipe)

let testData : obj [] [] = [|
  [| 9;    "5158916779" |]
  [| 5;    "0124515891" |]
  [| 18;   "9251071085" |]
  [| 2018; "5941429882" |]
|]

[<Theory>]
[<MemberData("testData")>]
let ``Next Ten Scores - Test`` lastRecipe expected =
  let actual = makeRecipes [| 3; 7 |] lastRecipe
  Assert.Equal (expected, actual)

[<Fact>]
let ``Next Ten Scores - Actual`` () =
  let actual = makeRecipes [| 3; 7 |] (File.ReadAllText "Day14input.txt" |> int)
  let expected = "1044257397"
  Assert.Equal (expected, actual)