module Day12

open System.Text.RegularExpressions
open System.IO
open Xunit
open System.Numerics

type Pot = Empty | Full with
  static member ofChar c =
    match c with
    | '.' -> Empty
    | '#' -> Full
    | _ -> failwith "Invalid input"

type State = {
  Input : Pot list
}
type Note = {
  Pattern : Pot list
  Produces : Pot
}

let stateRegex = Regex "initial state: ([.#]+)"
let noteRegex = Regex "([.#]{5}) => ([.#])"

let parseStateLine state = {
  Input = (stateRegex.Match state).Groups.[1].Value |> (Seq.map Pot.ofChar >> Seq.toList)
}

let parseNoteLine note =
  let m = noteRegex.Match note
  {
    Pattern = m.Groups.[1].Value |> (Seq.map Pot.ofChar >> Seq.toList)
    Produces = m.Groups.[2].Value |> (Seq.head >> Pot.ofChar)
  }

let parseInput inputLines =
  match inputLines with
  | state :: _ :: notes ->
    state |> parseStateLine,
    notes |> Seq.map parseNoteLine
  | _ -> failwith "Invalid input"

let msb n =
  if n = 0I then 0
  elif n = 1I then 1
  else BigInteger.Log(n, 2.) |> int |> (+) 1
let lsb n = msb (n &&& -n)

let seqBits state = seq { msb state - 1 .. -1 .. 0 }
let bitVal state b = (state >>> b) &&& 1I

let raster (state : bigint) =
  let rasterBit b = if bitVal state b = 0I then '.' else '#'
  state |> (seqBits >> Seq.map rasterBit >> Seq.toArray >> System.String)

let sum (start, state) =
  let bits = seqBits state |> Seq.cache
  let b0 = Seq.head bits
  let countBit b =
    let potVal = b0 - b + start
    let hasPlant = bitVal state b |> int
    potVal * hasPlant
  bits |> (Seq.map countBit >> Seq.sum)

let gen noteSize count (state, notes) =
  let noteMidpoint = noteSize / 2
  let noteShift = noteSize - 1
  let combinations = pown 2
  let mask size = combinations size - 1 |> bigint
  let noteMask = mask noteSize
  let potBit p = match p with Empty -> 0 | Full -> 1
  let inline bitFolder f v p = (v <<< 1) + f p
  let inline getMask cast = List.fold (bitFolder (potBit >> cast)) (cast 0)
  let state = state.Input |> getMask bigint
  let notes =
    let array = Array.zeroCreate<bigint> (combinations noteSize)
    let setNote n = array.[(getMask int) n.Pattern] <- potBit n.Produces |> bigint
    notes |> Seq.iter setNote
    array
  let produce (state : bigint) =
    //let stateDebug = raster state
    let shiftedState = state <<< noteShift
    //let shiftedStateDebug = raster shiftedState
    let produceBit b =
      let pattern = shiftedState >>> b
      let index = pattern &&& noteMask |> int
      notes.[index]
    let shiftedResult =
      seq { msb shiftedState + noteSize .. -1 .. 0 }
      |> (Seq.fold (bitFolder produceBit) 0I)
      <<< noteMidpoint
    //let shiftedResultDebug = raster shiftedResult
    let lsb0 = lsb (shiftedResult &&& (mask noteShift))
    let shiftBack = if lsb0 = 0 then noteShift else lsb0 - 1
    let result = shiftedResult >>> shiftBack
    //let resultDebug = raster result
    noteShift - shiftBack, result
  let originalLength = msb state
  let rec gen n start state =
    if n = count then originalLength - (msb state) + start, state
    else
      let shift, state' = produce state
      gen (n + 1) (start + shift) state'
  gen 0 0 state

let testInputData = [
  "initial state: #..#.#..##......###...###"
  ""
  "...## => #"
  "..#.. => #"
  ".#... => #"
  ".#.#. => #"
  ".#.## => #"
  ".##.. => #"
  ".#### => #"
  "#.#.# => #"
  "#.### => #"
  "##.#. => #"
  "##.## => #"
  "###.. => #"
  "###.# => #"
  "####. => #"
]

let testGenData : obj [] [] = [|
  [|  0;  0;   "#..#.#..##......###...###"           |]
  [|  1;  0;   "#...#....#.....#..#..#..#"           |]
  [|  2;  0;   "##..##...##....#..#..#..##"          |]
  [|  3; -1;  "#.#...#..#.#....#..#..#...#"          |]
  [|  4;  0;   "#.#..#...#.#...#..#..##..##"         |]
  [|  5;  1;    "#...##...#.#..#..#...#...#"         |]
  [|  6;  1;    "##.#.#....#...#..##..##..##"        |]
  [|  7;  0;   "#..###.#...##..#...#...#...#"        |]
  [|  8;  0;   "#....##.#.#.#..##..##..##..##"       |]
  [|  9;  0;   "##..#..#####....#...#...#...#"       |]
  [| 10; -1;  "#.#..#...#.##....##..##..##..##"      |]
  [| 11;  0;   "#...##...#.#...#.#...#...#...#"      |]
  [| 12;  0;   "##.#.#....#.#...#.#..##..##..##"     |]
  [| 13; -1;  "#..###.#....#.#...#....#...#...#"     |]
  [| 14; -1;  "#....##.#....#.#..##...##..##..##"    |]
  [| 15; -1;  "##..#..#.#....#....#..#.#...#...#"    |]
  [| 16; -2; "#.#..#...#.#...##...#...#.#..##..##"   |]
  [| 17; -1;  "#...##...#.#.#.#...##...#....#...#"   |]
  [| 18; -1;  "##.#.#....#####.#.#.#...##...##..##"  |]
  [| 19; -2; "#..###.#..#.#.#######.#.#.#..#.#...#"  |]
  [| 20; -2; "#....##....#####...#######....#.#..##" |]
|]

[<Theory>]
[<MemberData("testGenData")>]
let ``Plant Generation - Test`` count start plants =
  let actualStart, actualState = parseInput testInputData |> gen 5 count
  let actualPlants = raster actualState
  Assert.Equal (start, actualStart)
  Assert.Equal (plants, actualPlants)

[<Fact>]
let ``Plant Generation - Sum`` () =
  let actual = parseInput testInputData |> (gen 5 20 >> sum)
  let expected = 325
  Assert.Equal (expected, actual)

[<Fact>]
let ``Plant Generation - Actual`` () =
  let actual = File.ReadAllLines "Day12input.txt" |> (Array.toList >> parseInput >> gen 5 20 >> sum)
  let expected = 1672
  Assert.Equal (expected, actual)