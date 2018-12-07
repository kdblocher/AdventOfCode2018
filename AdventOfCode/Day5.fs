module Day5

open System
open System.IO
open Xunit

let processPolymer (unitStr : string) =
  let rec loop result units =
    let CASE_DIFF = 32
    match units, result with 
    | [], _ ->
      new String(result |> (List.map char >> List.rev >> Array.ofList))
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
  let actual = processPolymer input
  Assert.Equal<string> (expected, actual)

[<Fact>]
let ``Polymer Actual`` () =
  let actual = (File.ReadAllText "Day5input.txt") |> (processPolymer >> String.length)
  let expected = 10368
  Assert.Equal (expected, actual)