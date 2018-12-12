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

let converged (start1, state1) (start2, state2) =
  start1 + 1 = start2 && state1 = state2

type Generation = {
  Start : int
  State : bigint
  Converged : bigint option
  BaseSum : bigint
}

let baseSum { Start = start; State = state } =
  let bits = seqBits state |> Seq.cache
  let b0 = Seq.head bits
  let countBit b =
    let potVal = b0 - b + start
    let hasPlant = bitVal state b |> int
    potVal * hasPlant
  bits |> (Seq.map countBit >> Seq.sum) |> bigint

let convergedSum generations last nextToLast =
  match last, nextToLast with
  | { Converged = Some lastGeneration; BaseSum = sum2 }, { BaseSum = sum1 } ->
    BigInteger.Subtract(generations, lastGeneration) * (sum2 - sum1)
  | _ -> 0I

let sum count gens =
  let gens = gens |> Seq.rev |> Seq.toList
  match gens with last :: nextToLast :: tail -> convergedSum count last nextToLast | _ -> 0I
  + baseSum (List.head gens)

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
    let shiftedState = state <<< noteShift
    let produceBit b =
      let pattern = shiftedState >>> b
      let index = pattern &&& noteMask |> int
      notes.[index]
    let shiftedResult =
      seq { msb shiftedState + noteSize .. -1 .. 0 }
      |> (Seq.fold (bitFolder produceBit) 0I)
      <<< noteMidpoint
    let lsb0 = lsb (shiftedResult &&& (mask noteShift))
    let shiftBack = if lsb0 = 0 then noteShift else lsb0 - 1
    let result = shiftedResult >>> shiftBack
    noteShift - shiftBack, result
  let originalLength = msb state
  let rec gen n prevStart start prevState curState = seq {
    let hasConverged = converged (prevStart, prevState) (start, curState)
    let resultStart = originalLength - (msb curState) + start
    let result = {
      Start = resultStart
      State = curState
      Converged = if hasConverged then Some n else None
      BaseSum = 0I
    }
    let result = { result with BaseSum = baseSum result }
    yield result
    if n = count || hasConverged then ()
    else
      let shift, nextState = produce curState
      yield! gen (n + 1I) start (start + shift) curState nextState
  }
  gen 0I 0 0 0I state

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
  [|  0I;  0;   "#..#.#..##......###...###"           |]
  [|  1I;  0;   "#...#....#.....#..#..#..#"           |]
  [|  2I;  0;   "##..##...##....#..#..#..##"          |]
  [|  3I; -1;  "#.#...#..#.#....#..#..#...#"          |]
  [|  4I;  0;   "#.#..#...#.#...#..#..##..##"         |]
  [|  5I;  1;    "#...##...#.#..#..#...#...#"         |]
  [|  6I;  1;    "##.#.#....#...#..##..##..##"        |]
  [|  7I;  0;   "#..###.#...##..#...#...#...#"        |]
  [|  8I;  0;   "#....##.#.#.#..##..##..##..##"       |]
  [|  9I;  0;   "##..#..#####....#...#...#...#"       |]
  [| 10I; -1;  "#.#..#...#.##....##..##..##..##"      |]
  [| 11I;  0;   "#...##...#.#...#.#...#...#...#"      |]
  [| 12I;  0;   "##.#.#....#.#...#.#..##..##..##"     |]
  [| 13I; -1;  "#..###.#....#.#...#....#...#...#"     |]
  [| 14I; -1;  "#....##.#....#.#..##...##..##..##"    |]
  [| 15I; -1;  "##..#..#.#....#....#..#.#...#...#"    |]
  [| 16I; -2; "#.#..#...#.#...##...#...#.#..##..##"   |]
  [| 17I; -1;  "#...##...#.#.#.#...##...#....#...#"   |]
  [| 18I; -1;  "##.#.#....#####.#.#.#...##...##..##"  |]
  [| 19I; -2; "#..###.#..#.#.#######.#.#.#..#.#...#"  |]
  [| 20I; -2; "#....##....#####...#######....#.#..##" |]
|]

[<Theory>]
[<MemberData("testGenData")>]
let ``Plant Generation - Test`` generations start plants =
  let { Start = actualStart; State = actualState } = parseInput testInputData |> (gen 5 generations >> Seq.last)
  let actualPlants = raster actualState
  Assert.Equal (start, actualStart)
  Assert.Equal (plants, actualPlants)

[<Fact>]
let ``Plant Generation - Sum`` () =
  let generations = 20I
  let actual = parseInput testInputData |> (gen 5 generations >> sum generations)
  let expected = 325I
  Assert.Equal (expected, actual)

[<Fact>]
let ``Plant Generation - Actual`` () =
  let generations = 20I
  let actual = File.ReadAllLines "Day12input.txt" |> (Array.toList >> parseInput >> gen 5 generations >> sum generations)
  let expected = 1672I
  Assert.Equal (expected, actual)

[<Fact>]
let ``Plant Generation - Actual 2`` () =
  let generations = 50000000000I
  let actual = File.ReadAllLines "Day12input.txt" |> (Array.toList >> parseInput >> gen 5 generations >> sum generations)
  let expected = 1650000000055I
  Assert.True false
