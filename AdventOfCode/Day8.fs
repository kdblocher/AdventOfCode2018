module Day8

open Xunit
open System.IO

let parseInputText (input : string) =
  input.Split ' ' |> (Array.map int >> Array.toList)

type Node<'a> = {
  Metadata : 'a list
  Nodes : Node<'a> array
}

let makeTree license =
  let rec descend stream =
    match stream with
    | hCount :: mCount :: stream ->
      let nodes, stream = lateral hCount stream
      let m, stream = stream |> List.splitAt mCount
      { Metadata = m; Nodes = nodes |> List.toArray }, stream
    | _ -> failwith "data remaining in stream"
  and lateral count stream =
    if count = 0
      then [], stream
      else
        let node, stream = descend stream
        let nodes, stream = lateral (count - 1) stream
        node :: nodes, stream
  let node, stream = descend license
  if List.isEmpty stream
    then node
    else failwith "data remaining in stream"

let rec metadataSum tree =
  match tree with
  | { Metadata = ms; Nodes = ns } ->
    List.sum ms + (Array.sumBy metadataSum ns)

let rec metadataValue tree =
  match tree with
  | { Metadata = ms; Nodes = [||] } ->
    List.sum ms
  | { Metadata = ms; Nodes = ns } ->
    let getValue vals m =
      if m <= 0 || m > ns.Length || vals |> Map.containsKey m
        then vals
        else vals |> Map.add m (metadataValue ns.[m - 1])
    let vals = List.fold getValue Map.empty ms
    let sum n = vals |> (Map.tryFind n >> Option.defaultValue 0)
    ms |> List.sumBy sum

let treeTestData = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

[<Fact>]
let ``Metadata Sum - Test`` () =
  let actual = treeTestData |> (parseInputText >> makeTree >> metadataSum)
  let expected = 138
  Assert.Equal (expected, actual)

[<Fact>]
let ``Metadata Sum - Actual`` () =
  let actual = File.ReadAllText "Day8input.txt" |> (parseInputText >> makeTree >> metadataSum)
  let expected = 46962
  Assert.Equal (expected, actual)

[<Fact>]
let ``Metadata Value - Test`` () =
  let actual = treeTestData |> (parseInputText >> makeTree >> metadataValue)
  let expected = 66
  Assert.Equal (expected, actual)

[<Fact>]
let ``Metadata Value - Actual`` () =
  let actual = File.ReadAllText "Day8input.txt" |> (parseInputText >> makeTree >> metadataValue)
  let expected = 22633
  Assert.Equal (expected, actual)