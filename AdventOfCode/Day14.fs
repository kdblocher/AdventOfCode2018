module Day14

open System.Collections.Generic
open Helpers.LinkedList
open Xunit
open System.IO

let makeRecipes players =
  let ring = LinkedList players
  let value (n : LinkedListNode<_>) = n.Value
  let players =
    let nodeAt i _ =
      let n = ring.First
      clockwise ring i n
    players |> Array.mapi nodeAt
  let getNextRecipes i = seq { if i < 10 then yield i else yield i / 10; yield i % 10 }
  seq {
    yield! ring
    while true do
      let digits = players |> (Seq.sumBy value >> getNextRecipes) |> Seq.toArray
      digits |> Seq.iter (ring.AddLast >> ignore)
      players |> (Array.iteri (fun i r -> players.[i] <- clockwise ring (value r + 1) r))
      yield! digits
  }

let getScore lastRecipe recipes =
  let scoredRecipes = 10
  recipes |> (Seq.skip lastRecipe >> Seq.take scoredRecipes >> Seq.map (fun i -> i.ToString()) >> String.concat "")

let findOccurrence (input : string []) (recipes : 'a seq) =
  let e = recipes.GetEnumerator()
  let mutable matched = 0
  let mutable count = 0
  let check i = e.Current.ToString() = input.[i]
  while e.MoveNext() && matched < input.Length do
    if check matched then
      matched <- matched + 1
    else
      matched <- if check 0 then 1 else 0
    count <- count + 1
  count - matched

let find lastRecipe = findOccurrence (lastRecipe.ToString().ToCharArray() |> Array.map (fun n -> n.ToString()))

let testData : obj [] [] = [|
  [| 9;    "5158916779" |]
  [| 5;    "0124515891" |]
  [| 18;   "9251071085" |]
  [| 2018; "5941429882" |]
|]

[<Theory>]
[<MemberData("testData")>]
let ``Next Ten Scores - Test`` lastRecipe expected =
  let actual = makeRecipes [| 3; 7 |] |> getScore lastRecipe
  Assert.Equal (expected, actual)

[<Fact>]
let ``Next Ten Scores - Actual`` () =
  let lastRecipe = File.ReadAllText "Day14input.txt" |> int
  let actual = makeRecipes [| 3; 7 |] |> getScore lastRecipe
  let expected = "1044257397"
  Assert.Equal (expected, actual)

let findTestData : obj [] [] = [|
  [| "51589"; 9    |]
  [| "01245"; 5    |]
  [| "92510"; 18   |]
  [| "59414"; 2018 |]
|]

[<Theory>]
[<MemberData("findTestData")>]
let ``Occurrence - Test`` sequence expected =
  let actual = makeRecipes [| 3; 7 |] |> find sequence
  Assert.Equal (expected, actual)

[<Fact>]
let ``Occurrence - Actual`` () =
  let sequence = File.ReadAllText "Day14input.txt" |> int
  let actual = makeRecipes [| 3; 7 |] |> find sequence
  let expected = 20185425
  Assert.Equal (expected, actual)