module Day5

open System
open System.IO
open Xunit

let processPolymer (unitsToRemove : char list) (unitStr : string) =
  let unitsToRemove = unitsToRemove |> List.map int
  let rec loop result units =
    let CASE_DIFF = 32
    match units, result with 
    | [], _ ->
      new String(result |> (List.map char >> List.rev >> Array.ofList))
    | u :: units, _ when unitsToRemove |> List.contains u ->
      loop result units
    | u :: units, r :: result when u + CASE_DIFF = r || u - CASE_DIFF = r ->
      loop result units
    | u :: units, _ ->
      loop (u :: result) units
  loop [] (unitStr |> Seq.map int |> Seq.toList)

let polymerTestUnits = [|
  [| "aA"; "" |]
  [| "abBA"; "" |]
  [| "abAB"; "abAB" |]
  [| "aabAAB"; "aabAAB" |]
  [| "dabAcCaCBAcCcaDA"; "dabCBAcaDA" |]
|]

[<Theory>]
[<MemberData("polymerTestUnits")>]
let ``Polymer Tests`` input expected =
  let actual = processPolymer [] input
  Assert.Equal<string> (expected, actual)

[<Fact>]
let ``Polymer Actual`` () =
  let actual = (File.ReadAllText "Day5input.txt") |> (processPolymer [] >> String.length)
  let expected = 10368
  Assert.Equal (expected, actual)

let polymerWithRemovalTestUnits : obj [] [] = [|
  [| "dabAcCaCBAcCcaDA"; [ 'A'; 'a' ]; "dbCBcD" |]
  [| "dabAcCaCBAcCcaDA"; [ 'B'; 'b' ]; "daCAcaDA" |]
  [| "dabAcCaCBAcCcaDA"; [ 'C'; 'c' ]; "daDA" |]
  [| "dabAcCaCBAcCcaDA"; [ 'D'; 'd' ]; "abCBAc" |]
|]

let findShortestPolymerLength input =
  let pp s u = processPolymer u s
  Seq.zip (seq { 'a' .. 'z' }) (seq { 'A' .. 'Z' })
  |> (Seq.map (fun (a, b) -> [ a; b ]) >> Seq.map (pp input >> String.length) >> Seq.min)

[<Theory>]
[<MemberData("polymerWithRemovalTestUnits")>]
let ``Polymer Removal Tests`` input removal expected =
  let actual = processPolymer removal input
  Assert.Equal<string> (expected, actual)

[<Fact>]
let ``Polymer Removal Tests - Find Shortest`` () =
  let actual = (polymerWithRemovalTestUnits.[0].[0] :?> string) |> findShortestPolymerLength
  let expected = 4
  Assert.Equal (expected, actual)

[<Fact>]
let ``Polymer Removal Actual`` () =
  let actual = (File.ReadAllText "Day5input.txt") |> findShortestPolymerLength
  let expected = 4122
  Assert.Equal (expected, actual)