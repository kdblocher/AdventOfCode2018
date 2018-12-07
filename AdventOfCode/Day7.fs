module Day7

open System
open System.Text.RegularExpressions
open Xunit
open System.IO

let parseInputLineRegex = Regex("Step (.) must be finished before step (.) can begin")
let parseInputLine input =
  let m = parseInputLineRegex.Match input
  let getValue i = m.Groups.[i + 1].Value
  (getValue 0, getValue 1)

let stepTestParseData = [|
  "Step C must be finished before step A can begin."
  "Step C must be finished before step F can begin."
  "Step A must be finished before step B can begin."
  "Step A must be finished before step D can begin."
  "Step B must be finished before step E can begin."
  "Step D must be finished before step E can begin."
  "Step F must be finished before step E can begin."
|]

let stepTestData : obj [] [] = [|
  [| stepTestParseData.[0]; [| "C"; "A" |] |]
  [| stepTestParseData.[1]; [| "C"; "F" |] |]
  [| stepTestParseData.[2]; [| "A"; "B" |] |]
  [| stepTestParseData.[3]; [| "A"; "D" |] |]
  [| stepTestParseData.[4]; [| "B"; "E" |] |]
  [| stepTestParseData.[5]; [| "D"; "E" |] |]
  [| stepTestParseData.[6]; [| "F"; "E" |] |]
|]

[<Theory>]
[<MemberData("stepTestData")>]
let ``Instruction Order Test - Parse`` input (edge : string array) =
  let actual = parseInputLine input
  Assert.Equal ((edge.[0], edge.[1]), actual)

let inline reversePair (a, b) = b, a

let getGraph pairs =
  let getEdgeMap = List.groupBy fst >> List.map (fun (a, b) -> a, List.map snd b |> set) >> Map
  let vertices = pairs |> (List.collect (fun (a, b) -> [ a; b ]) >> set)
  let sources = Set.difference vertices (pairs |> (Seq.map snd >> set))
  let edges = pairs |> getEdgeMap
  let prereqs = pairs |> (getEdgeMap << List.map reversePair)
  vertices, sources, edges, prereqs

let topologicalSort pairs =
  let (_, sources, edges, prereqs) = getGraph pairs
  let rec loop visited prereqs vertices =
    match vertices with
    | [] -> []
    | v :: vs ->
      if Set.contains v visited || Map.containsKey v prereqs
        then loop visited prereqs vs
        else
          let prereqs = prereqs |> (Map.map (fun _ -> Set.remove v) >> Map.filter (fun _ -> not << Set.isEmpty))
          let newVertices = edges |> (Map.tryFind v >> Option.defaultValue Set.empty >> Set.filter (fun v -> not (Map.containsKey v prereqs)) >> Set.toList)
          v :: loop (Set.add v visited) prereqs (newVertices @ vs |> List.sort)
  let sources = sources |> Set.toList |> List.sort
  loop Set.empty prereqs sources

let runSort = Seq.map parseInputLine >> Seq.toList >> topologicalSort >> String.concat ""

[<Fact>]
let ``Instruction Order Test`` () =
  let actual = stepTestParseData |> runSort
  let expected = "CABDFE"
  Assert.Equal (expected, actual)

[<Fact>]
let ``Instruction Order Actual`` () =
  let actual = File.ReadAllLines "Day7input.txt" |> runSort
  let expected = "EUGJKYFQSCLTWXNIZMAPVORDBH"
  Assert.Equal (expected, actual)

let makespan workerCount (getCost : 'a -> int) pairs =
  let (_, sources, edges, prereqs) = getGraph pairs
  let rec loop time visited running prereqs vertices =
    match vertices with
      | [] when Map.isEmpty running -> time
      | v :: vertices when Map.count running < workerCount ->
        if Set.contains v visited || Map.containsKey v prereqs
          then loop time visited running prereqs vertices
          else loop time (Set.add v visited) (running |> Map.add v (getCost v)) prereqs vertices
      | _ ->
        let (v, t) = running |> (Map.toSeq >> Seq.minBy snd)
        let prereqs = prereqs |> (Map.map (fun _ -> Set.remove v) >> Map.filter (fun _ -> not << Set.isEmpty))
        let newVertices = edges |> (Map.tryFind v >> Option.defaultValue Set.empty >> Set.filter (fun v -> not (Map.containsKey v prereqs)) >> Set.toList)
        let running = (running |> Map.remove v) |> Map.map (fun _ u -> u - t)
        loop (time + t) visited running prereqs (newVertices @ vertices |> List.sort)
      
  let sources = sources |> Set.toList |> List.sort
  loop 0 Set.empty Map.empty prereqs sources

[<Fact>]
let ``Makespan Test`` () =
  let actual = stepTestParseData |> (Seq.map parseInputLine >> Seq.toList >> makespan 2 (fun v -> int v.[0] - 64))
  let expected = 15
  Assert.Equal (expected, actual)

[<Fact>]
let ``Makespan Actual`` () =
  let actual = File.ReadAllLines "Day7input.txt" |> (Seq.map parseInputLine >> Seq.toList >> makespan 2 (fun v -> int v.[0] - 4))
  let expected = 15
  Assert.Equal (expected, actual)