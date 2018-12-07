module Day6

open Xunit
open System.Text
open System.IO

let parseInputLine (input : string) =
  input.Split ", " |> (Array.map int >> (fun s -> s.[0], s.[1]))

type Location = {
  ID : int option
  Distance : int
}
type Event = {
  Location : Location
  Coordinate : int * int
}

let inline setup coords =
  let xs = coords |> Seq.map fst
  let ys = coords |> Seq.map snd
  let minX = Seq.min xs
  let minY = Seq.min ys
  let maxX = Seq.max xs
  let maxY = Seq.max ys
  let lengthX = maxX - minX + 1
  let lengthY = maxY - minY + 1
  let normalize min p = p - min
  let xs = xs |> Seq.map (normalize minX)
  let ys = ys |> Seq.map (normalize minY)
  let coords = Seq.zip xs ys |> Seq.toList
  let array = Array.init lengthX (fun _ -> Array.zeroCreate lengthY)
  (coords, array, (lengthX, lengthY))

let inline checkBounds (lengthX, lengthY) onOutOfBounds coord =
  match coord with
    | (x, _) when (x < 0 || x >= lengthX) ->
      onOutOfBounds ()
      false
    | (_, y) when (y < 0 || y >= lengthY) ->
      onOutOfBounds ()
      false
    | _ -> true

let inline processEvent update (x, y) =
  (Seq.map update >> Seq.choose id >> Seq.toList) [
    x - 1, y
    x + 1, y
    x, y - 1
    x, y + 1
  ]

let rec processAllEvents processor events =
  match List.collect processor events with
  | [] -> ()
  | events -> processAllEvents processor events

let maxArea coords =
  let (coords, array, (lengthX, lengthY)) = setup coords
  let areas = Array.init (List.length coords) (fun _ -> Some 1)
  let setStartLocations i (x, y) = array.[x].[y] <- Some { ID = Some i; Distance = 0 }
  coords |> Seq.iteri setStartLocations

  let setLocation loc (x, y) =
    let updateArea locid f =
      locid |> Option.iter (fun id -> areas.[id] <- f areas.[id])
    checkBounds (lengthX, lengthY) (fun () -> updateArea loc.ID (fun _ -> None)) (x, y) &&
      match array.[x].[y] with
      | None ->
        array.[x].[y] <- Some loc;
        updateArea loc.ID (Option.map (fun x -> x + 1))
        true
      | Some { ID = id2 } when loc.ID = id2 ->
        false
      | Some { ID = id2; Distance = d2 } when loc.Distance = d2 ->
        array.[x].[y] <- Some { loc with ID = None }
        updateArea id2 (Option.map (fun x -> x - 1))
        false
      | _ ->
        false

  let p event =
    match event with
    | { Location = loc; Coordinate = coord } ->
      let update coord =
        if setLocation loc coord
          then Some { Location = { loc with Distance = loc.Distance + 1 }; Coordinate = coord }
          else None
      processEvent update coord

  let makeInitialEvent i coord = { Location = { ID = Some i; Distance = 0 }; Coordinate = coord }
  (List.mapi makeInitialEvent >> processAllEvents p) coords

  (Array.choose id >> Array.max) areas

let coordinateTestData = [|
  "1, 1"
  "1, 6"
  "8, 3"
  "3, 4"
  "5, 5"
  "8, 9"
|]

[<Fact>]
let ``Max Area Test`` () =
  let actual = coordinateTestData |> (Seq.map parseInputLine >> maxArea)
  let expected = 17
  Assert.Equal (expected, actual)

[<Fact>]
let ``Max Area Actual`` () =
  let actual = File.ReadAllLines "Day6input.txt" |> (Seq.map parseInputLine >> maxArea)
  let expected = 4171
  Assert.Equal (expected, actual)

let safeRegion maxDistance coords =
  let (coords, array, (lengthX, lengthY)) = setup coords
  let mutable size = 0
  let checkDist (x, y) =
    checkBounds (lengthX, lengthY) ignore (x, y) &&
      let calc (x0, y0) = abs (x - x0) + abs (y - y0)
      match array.[x].[y] with
      | Some _ ->
        false
      | None ->
        let inRegion = (coords |> Seq.sumBy calc) < maxDistance
        array.[x].[y] <- Some inRegion
        if inRegion then size <- size + 1
        inRegion
    
  let p coord =
    let update coord =
      if checkDist coord then Some coord else None
    processEvent update coord
  
  (lengthX / 2, lengthY / 2) |> List.singleton |> processAllEvents p
  size

[<Fact>]
let ``Safe Region Test`` () =
  let actual = coordinateTestData |> (Seq.map parseInputLine >> safeRegion 32)
  let expected = 16
  Assert.Equal (expected, actual)

[<Fact>]
let ``Safe Region Actual`` () =
  let actual = File.ReadAllLines "Day6input.txt" |> (Seq.map parseInputLine >> safeRegion 10000)
  let expected = 39545
  Assert.Equal (expected, actual)