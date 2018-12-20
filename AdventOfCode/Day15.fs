module Day15

open Helpers.Queue
open Xunit
open Helpers
open System.IO

type Tile = Walls | Floor
type Race = Elf | Goblin
type Point = int * int
type Unit = { Race : Race; HP : int; Power : int }

type CombatState = Ongoing | Ended

type CombatRoundResult = {
  Round : int
  Units : Map<Point, Unit>
  State : CombatState
}

let makeUnit race = { Race = race; HP = 200; Power = 3 }
let hp (_, u) = u.HP

let parseInput input =
  let mutable units = Map.empty
  let parseLine i line =
    let parseChar j char =
      let addUnit race = units |> Map.add (i, j) (makeUnit race)
      match char with
      | '#'  -> Walls
      | '.'  -> Floor
      | 'G'  -> units <- addUnit Goblin ; Floor
      | 'E'  -> units <- addUnit Elf    ; Floor
      | _    -> failwith "Invalid input"
    line |> (Seq.mapi parseChar >> Seq.toArray)
  let tiles = input |> (Seq.mapi parseLine >> Seq.toArray)
  tiles, units

let neighbors (tiles : Tile [] []) allies (x, y) =
  seq {
    if x >= 0               then yield x - 1, y
    if y >= 0               then yield x, y - 1
    if y < tiles.[0].Length then yield x, y + 1
    if x < tiles.Length     then yield x + 1, y
  } |> Seq.filter (fun (x, y) ->
    tiles.[x].[y] = Floor && (allies |> Map.containsKey (x, y) |> not))

let turnOrder = Map.toSeq >> Seq.sort

let partitionUnits unit = Map.partition (fun _ t -> t.Race = unit.Race)

let checkState units maybeState unit =
  let oneSided = partitionUnits unit units |> (snd >> Map.isEmpty)
  match maybeState with
  | None -> (if oneSided then Ended else Ongoing)
  | Some Ongoing when oneSided -> Ended
  | Some x -> x

let moveUnit tiles units p0 unit =
  let allies, enemies = partitionUnits unit units
  let neighbors = neighbors tiles allies
  let move =
    let rec visit queue visited =
      let body ((p, direction), queue) =
        if visited |> Set.contains p then
          visit queue visited 
        elif enemies |> (Map.tryFind p >> Option.isSome) then
          direction
        else
          let direction = direction |> (Option.orElseWith (fun () -> if p = p0 then None else Some p))
          let queue = p |> (neighbors >> Seq.map (fun p -> p, direction) >> Seq.fold Queue.enqueue queue)
          let visited = visited |> Set.add p
          visit queue visited
      queue |> Queue.tryDequeue |> Option.bind body
    visit ((p0, None) |> (Seq.singleton >> Queue.ofSeq)) Set.empty
  let update units p = p, units |> (Map.remove p0 >> Map.add p unit)
  move |> Option.map (update units)

let attackWithUnit tiles units p unit =
  let allies, enemies = partitionUnits unit units
  let neighbors = neighbors tiles allies
  let targetPoints = neighbors p |> (Seq.filter (enemies.ContainsKey) >> Seq.cache)
  let pickTarget () =
    let targets = targetPoints |> Seq.map (fun p -> p, Map.find p enemies)
    let leastHP = targets |> (Seq.map hp >> Seq.min)
    targets |> (Seq.filter (hp >> (=) leastHP) >> Seq.head)
  let dealDamage (p, u) =
    let u = { u with HP = u.HP - unit.Power }
    if u.HP <= 0 then
      (Some (p, u)), units |> Map.remove p
    else
      None, units |> Map.add p u
  Map.tryFind p allies
  |> Option.bind (fun _ -> targetPoints |> (Seq.tryHead >> Option.map (ignore >> pickTarget >> dealDamage)))


let combatRound tiles units suddenDeath =
  let turn (state, units) (p, unit) =
    let state = checkState units state unit
    if state <> Ongoing then Some state, units
    else
      let p, units = moveUnit       tiles units p unit |> Option.defaultValue (p, units)
      let r, units = attackWithUnit tiles units p unit |> Option.defaultValue (None, units)
      match suddenDeath with
      | Some race when r |> (Option.map (fun (p, u) -> u.Race = race) >> Option.defaultValue false) ->
        Some Ended, units
      | _ -> Some state, units
  units |> (turnOrder >> Seq.fold turn (None, units))

let combat suddenDeath (tiles, units) =
  let mutable round = 0
  let mutable state = Ongoing
  let mutable units = units
  seq {
    yield { Round = round; State = state; Units = units }
    while state = Ongoing do
      let state', units' = combatRound tiles units suddenDeath
      state <- state' |> Option.get
      units <- units'
      round <- round + 1
      yield { Round = round; State = state; Units = units }
  }

let outcome { Round = round; Units = units; } =
  let hpSum = units |> (Map.toSeq >> Seq.map hp >> Seq.sum)
  hpSum * (round - 1)

let search race (tiles, units) =
  let rec loop lower maybeUpper guess =
    let setPower _ u = if u.Race = race then { u with Power = guess } else u
    let result = combat (Some race) (tiles, units |> Map.map setPower) |> Seq.last
    let anyDeaths = result.Units |> Map.exists (fun _ u -> u.Race <> race)
    match anyDeaths, maybeUpper with
    | false, Some upper when upper - lower <= 1 -> outcome result
    | true, None -> loop guess None (guess * 2)
    | true, Some upper -> loop guess (Some upper) ((upper + guess) / 2)
    | _ -> loop lower (Some guess) ((guess + lower + 1) / 2)
  loop 1 None 1

[<Fact>]
let ``Turn Order`` () =
  let input = [|
    "#######"
    "#.G.E.#"
    "#E.G.E#"
    "#.G.E.#"
    "#######"
  |]
  let actual = input |> (parseInput >> snd >> turnOrder >> Seq.map fst)
  let expected = [|
    (1, 2); (1, 4)
    (2, 1); (2, 3); (2, 5)
    (3, 2); (3, 4)
  |]
  Assert.Equal (expected, actual)

[<Fact>]
let ``Movement 1`` () =
  let input = [|
    "#######"
    "#E..G.#"
    "#...#.#"
    "#.G.#G#"
    "#######"
  |]
  let tiles, units = parseInput input
  let p = 1, 1
  let unit = units |> Map.find p
  let actual = moveUnit tiles units p unit |> Option.map fst
  let expected = Some (1, 2)
  Assert.Equal (expected, actual)

[<Fact>]
let ``Movement 2`` () =
  let input = [|
    "#######"
    "#.E...#"
    "#.....#"
    "#...G.#"
    "#######"
  |]
  let tiles, units = parseInput input
  let p = 1, 2
  let unit = units |> Map.find p
  let actual = moveUnit tiles units p unit |> Option.map fst
  let expected = Some (1, 3)
  Assert.Equal (expected, actual)

[<Fact>]
let ``Movement 3`` () =
  let input = [|
    "#######"
    "#EE...#"
    "#E....#"
    "#..G..#"
    "#######"
  |]
  let tiles, units = parseInput input
  let p = 1, 1
  let unit = units |> Map.find p
  let actual = moveUnit tiles units p unit |> Option.map fst
  let expected = None
  Assert.Equal (expected, actual)

let moveSource = [|
  [|
    "#########"
    "#G..G..G#"
    "#.......#"
    "#.......#"
    "#G..E..G#"
    "#.......#"
    "#.......#"
    "#G..G..G#"
    "#########"
  |]
  [|
    "#########"
    "#.G...G.#"
    "#...G...#"
    "#...E..G#"
    "#.G.....#"
    "#.......#"
    "#G..G..G#"
    "#.......#"
    "#########"
  |]
  [|
    "#########"
    "#..G.G..#"
    "#...G...#"
    "#.G.E.G.#"
    "#.......#"
    "#G..G..G#"
    "#.......#"
    "#.......#"
    "#########"
  |]
  [|
    "#########"
    "#.......#"
    "#..GGG..#"
    "#..GEG..#"
    "#G..G...#"
    "#......G#"
    "#.......#"
    "#.......#"
    "#########"
  |]
|]

let moveData : obj [] [] = [|
  [| moveSource.[0]; moveSource.[1] |]
  [| moveSource.[1]; moveSource.[2] |]
  [| moveSource.[2]; moveSource.[3] |]
|]

[<Theory>]
[<MemberData("moveData")>]
let ``Move All`` input output =
  let tiles, units = parseInput input
  let getPoints (_, units) = units |> (Map.toSeq >> Seq.map fst)
  let actual = combatRound tiles units None |> getPoints
  let expected = output |> (parseInput >> getPoints)
  Assert.Equal<seq<_>> (expected, actual)

[<Fact>]
let ``Attack`` () =
  let tiles = Array.replicate 5 (Array.replicate 5 Floor)
  let setHP hp u = { u with HP = hp }
  let p, unit = (2, 2), makeUnit Elf
  let units = Map<Point, Unit> [
    (0, 0), makeUnit Goblin |> setHP 9
    (1, 2), makeUnit Goblin |> setHP 4
    p, unit
    (2, 3), makeUnit Goblin |> setHP 2
    (3, 2), makeUnit Goblin |> setHP 2
    (4, 3), makeUnit Goblin |> setHP 1
  ]
  let expected = Some (Some ((2, 3), makeUnit Goblin |> setHP -1), units |> Map.remove (2, 3))
  let actual = attackWithUnit tiles units p unit
  Assert.Equal (expected, actual)

let combatRoundInput =
  [|
    "#######"
    "#.G...#"
    "#...EG#"
    "#.#.#G#"
    "#..G#E#"
    "#.....#"
    "#######"
  |]
let combatRoundTestData : obj [] [] = [|
  [|
    0
    Map<Point, Unit> [
      (1, 2), { Race = Goblin; HP = 200; Power = 3 }
      (2, 4), { Race = Elf;    HP = 200; Power = 3 }
      (2, 5), { Race = Goblin; HP = 200; Power = 3 }
      (3, 5), { Race = Goblin; HP = 200; Power = 3 }
      (4, 3), { Race = Goblin; HP = 200; Power = 3 }
      (4, 5), { Race = Elf;    HP = 200; Power = 3 }
    ]
  |]
  [|
    1
    Map<Point, Unit> [
      (1, 3), { Race = Goblin; HP = 200; Power = 3 }
      (2, 4), { Race = Elf;    HP = 197; Power = 3 }
      (2, 5), { Race = Goblin; HP = 197; Power = 3 }
      (3, 3), { Race = Goblin; HP = 200; Power = 3 }
      (3, 5), { Race = Goblin; HP = 197; Power = 3 }
      (4, 5), { Race = Elf;    HP = 197; Power = 3 }
    ]
  |]
  [|
    2
    Map<Point, Unit> [
      (1, 4), { Race = Goblin; HP = 200; Power = 3 }
      (2, 3), { Race = Goblin; HP = 200; Power = 3 }
      (2, 4), { Race = Elf;    HP = 188; Power = 3 }
      (2, 5), { Race = Goblin; HP = 194; Power = 3 }
      (3, 5), { Race = Goblin; HP = 194; Power = 3 }
      (4, 5), { Race = Elf;    HP = 194; Power = 3 }
    ]
  |]
  [|
    23
    Map<Point, Unit> [
      (1, 4), { Race = Goblin; HP = 200; Power = 3 }
      (2, 3), { Race = Goblin; HP = 200; Power = 3 }
      (2, 5), { Race = Goblin; HP = 131; Power = 3 }
      (3, 5), { Race = Goblin; HP = 131; Power = 3 }
      (4, 5), { Race = Elf;    HP = 131; Power = 3 }
    ]
  |]
  [|
    24
    Map<Point, Unit> [
      (1, 3), { Race = Goblin; HP = 200; Power = 3 }
      (2, 4), { Race = Goblin; HP = 131; Power = 3 }
      (3, 3), { Race = Goblin; HP = 200; Power = 3 }
      (3, 5), { Race = Goblin; HP = 128; Power = 3 }
      (4, 5), { Race = Elf;    HP = 128; Power = 3 }
    ]
  |]
  [|
    25
    Map<Point, Unit> [
      (1, 2), { Race = Goblin; HP = 200; Power = 3 }
      (2, 3), { Race = Goblin; HP = 131; Power = 3 }
      (3, 5), { Race = Goblin; HP = 125; Power = 3 }
      (4, 3), { Race = Goblin; HP = 200; Power = 3 }
      (4, 5), { Race = Elf;    HP = 125; Power = 3 }
    ]
  |]
  [|
    26
    Map<Point, Unit> [
      (1, 1), { Race = Goblin; HP = 200; Power = 3 }
      (2, 2), { Race = Goblin; HP = 131; Power = 3 }
      (3, 5), { Race = Goblin; HP = 122; Power = 3 }
      (4, 5), { Race = Elf;    HP = 122; Power = 3 }
      (5, 3), { Race = Goblin; HP = 200; Power = 3 }
    ]
  |]
  [|
    27
    Map<Point, Unit> [
      (1, 1), { Race = Goblin; HP = 200; Power = 3 }
      (2, 2), { Race = Goblin; HP = 131; Power = 3 }
      (3, 5), { Race = Goblin; HP = 119; Power = 3 }
      (4, 5), { Race = Elf;    HP = 119; Power = 3 }
      (5, 4), { Race = Goblin; HP = 200; Power = 3 }
    ]
  |]
  [|
    28
    Map<Point, Unit> [
      (1, 1), { Race = Goblin; HP = 200; Power = 3 }
      (2, 2), { Race = Goblin; HP = 131; Power = 3 }
      (3, 5), { Race = Goblin; HP = 116; Power = 3 }
      (4, 5), { Race = Elf;    HP = 113; Power = 3 }
      (5, 5), { Race = Goblin; HP = 200; Power = 3 }
    ]
  |]
  [|
    47
    Map<Point, Unit> [
      (1, 1), { Race = Goblin; HP = 200; Power = 3 }
      (2, 2), { Race = Goblin; HP = 131; Power = 3 }
      (3, 5), { Race = Goblin; HP = 59;  Power = 3 }
      (5, 5), { Race = Goblin; HP = 200; Power = 3 }
    ]
  |]
|]

[<Theory>]
[<MemberData("combatRoundTestData")>]
let ``Combat Rounds - Test`` round units =
  let { Round = actualRound; Units = actualUnits } = combatRoundInput |> (parseInput >> combat None >> Seq.skip round >> Seq.head)
  Assert.Equal (round, actualRound)
  Assert.Equal<Map<_,_>> (units, actualUnits)

let combatOutcomeTestData : obj [] [] = [|
  [|
    [|
      "#######"
      "#.G...#"
      "#...EG#"
      "#.#.#G#"
      "#..G#E#"
      "#.....#"
      "#######"
    |]
    Map<Point, Unit> [
      (1, 1), { Race = Goblin; HP = 200; Power = 3 }
      (2, 2), { Race = Goblin; HP = 131; Power = 3 }
      (3, 5), { Race = Goblin; HP = 59;  Power = 3 }
      (5, 5), { Race = Goblin; HP = 200; Power = 3 }
    ]
    27730
  |]
  [|
    [|
      "#######"
      "#G..#E#"
      "#E#E.E#"
      "#G.##.#"
      "#...#E#"
      "#...E.#"
      "#######"
    |]
    Map<Point, Unit> [
      (1, 5), { Race = Elf; HP = 200; Power = 3 }
      (2, 1), { Race = Elf; HP = 197; Power = 3 }
      (3, 2), { Race = Elf; HP = 185; Power = 3 }
      (4, 1), { Race = Elf; HP = 200; Power = 3 }
      (4, 5), { Race = Elf; HP = 200; Power = 3 }
    ]
    36334
  |]
  [|
    [|
      "#######"
      "#E..EG#"
      "#.#G.E#"
      "#E.##E#"
      "#G..#.#"
      "#..E#.#"
      "#######"
    |]
    Map<Point, Unit> [
      (1, 2), { Race = Elf; HP = 164; Power = 3 }
      (1, 4), { Race = Elf; HP = 197; Power = 3 }
      (2, 3), { Race = Elf; HP = 200; Power = 3 }
      (3, 1), { Race = Elf; HP = 98 ; Power = 3 }
      (4, 2), { Race = Elf; HP = 200; Power = 3 }
    ]
    39514
  |]
  [|
    [|
      "#######"
      "#E.G#.#"
      "#.#G..#"
      "#G.#.G#"
      "#G..#.#"
      "#...E.#"
      "#######"
    |]
    Map<Point, Unit> [
      (1, 1), { Race = Goblin; HP = 200; Power = 3 }
      (1, 3), { Race = Goblin; HP = 98 ; Power = 3 }
      (2, 3), { Race = Goblin; HP = 200; Power = 3 }
      (4, 5), { Race = Goblin; HP = 95 ; Power = 3 }
      (5, 4), { Race = Goblin; HP = 200; Power = 3 }
    ]
    27755
  |]
  [|
    [|
      "#######"
      "#.E...#"
      "#.#..G#"
      "#.###.#"
      "#E#G#G#"
      "#...#G#"
      "#######"
    |]
    Map<Point, Unit> [
      (2, 3), { Race = Goblin; HP = 200; Power = 3 }
      (5, 1), { Race = Goblin; HP = 98 ; Power = 3 }
      (5, 3), { Race = Goblin; HP = 38 ; Power = 3 }
      (5, 5), { Race = Goblin; HP = 200; Power = 3 }
    ]
    28944
  |]
  [|
    [|
      "#########"
      "#G......#"
      "#.E.#...#"
      "#..##..G#"
      "#...##..#"
      "#...#...#"
      "#.G...G.#"
      "#.....G.#"
      "#########"
    |]
    Map<Point, Unit> [
      (1, 2), { Race = Goblin; HP = 137; Power = 3 }
      (2, 1), { Race = Goblin; HP = 200; Power = 3 }
      (2, 3), { Race = Goblin; HP = 200; Power = 3 }
      (3, 2), { Race = Goblin; HP = 200; Power = 3 }
      (5, 2), { Race = Goblin; HP = 200; Power = 3 }
    ]
    18740
  |]
|]

[<Theory>]
[<MemberData("combatOutcomeTestData")>]
let ``Combat Outcome - Test`` input units score =
  let lastRound = input |> (parseInput >> combat None >> Seq.last)
  let actualOutcome = outcome lastRound
  Assert.Equal<Map<_,_>> (units, lastRound.Units)
  Assert.Equal (score, actualOutcome)

[<Fact>]
let ``Combat Outcome - Actual`` () =
  let actual = File.ReadAllLines "Day15input.txt" |> (parseInput >> combat None >> Seq.last >> outcome)
  let expected = 229950
  Assert.Equal (expected, actual)

[<Fact>]
let ``Search - Test`` () =
  let actual = combatRoundInput |> (parseInput >> search Elf)
  let expected = 4988
  Assert.Equal (expected, actual)

[<Fact>]
let ``Search - Actual`` () =
  let actual = File.ReadAllLines "Day15input.txt" |> (parseInput >> search Elf)
  let expected = 54360
  Assert.Equal (expected, actual)
  