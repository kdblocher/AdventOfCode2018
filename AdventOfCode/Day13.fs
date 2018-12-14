module Day13

open Xunit
open System.IO

type Tile =
| Intersection
| Horizontal
| Vertical
| RightCurve
| LeftCurve

type CartDirection =
| Up
| Down
| Left
| Right

type TurnDirection =
| TurnLeft
| GoStraight
| TurnRight

type Cart = {
  Direction : CartDirection
  NextTurn : TurnDirection
  Collided : bool
}

type Mine = {
  Tiles : Tile option [] []
  Carts : Map<int * int, Cart>
}

let parseInput (input : #seq<string>) =
  let mutable carts = Map.empty
  let parseLine i line =
    let parseChar j char =
      let addCart c = carts <- carts |> Map.add (i, j) { Direction = c; NextTurn = TurnLeft; Collided = false }
      match char with
      | ' '  -> None
      | '-'  -> Some Horizontal
      | '|'  -> Some Vertical
      | '/'  -> Some RightCurve
      | '\\' -> Some LeftCurve
      | '+'  -> Some Intersection
      | '^'  -> addCart Up;    Some Vertical
      | 'v'  -> addCart Down;  Some Vertical
      | '<'  -> addCart Left;  Some Horizontal
      | '>'  -> addCart Right; Some Horizontal
      | _    -> failwith "Invalid input"
    line |> (Seq.mapi parseChar >> Seq.toArray)
  let tiles = input |> (Seq.mapi parseLine >> Seq.toArray)
  { Tiles = tiles; Carts = carts }

let moveTick { Tiles = tiles; Carts = carts } =
  let moveCart carts ((x, y), cart) =
    if cart.Collided || carts |> (Map.tryFind (x, y) >> Option.map (fun c -> c.Collided) >> Option.defaultValue false) then
      carts
    else
      let (x', y') =
        match cart.Direction with
        | Up    -> x - 1, y
        | Down  -> x + 1, y
        | Left  -> x, y - 1
        | Right -> x, y + 1
      let tile = tiles.[x'].[y']
      let direction = 
        match tile with
        | None -> failwith "Invalid move"
        | Some Horizontal | Some Vertical -> cart.Direction
        | Some RightCurve ->
          match cart.Direction with
          | Up    -> Right
          | Right -> Up
          | Left  -> Down
          | Down  -> Left
        | Some LeftCurve ->
          match cart.Direction with
          | Up    -> Left
          | Left  -> Up
          | Right -> Down
          | Down  -> Right
        | Some Intersection ->
          match cart.NextTurn, cart.Direction with
          | GoStraight, _ -> cart.Direction
          | TurnLeft,  Up    -> Left
          | TurnLeft,  Left  -> Down
          | TurnLeft,  Down  -> Right
          | TurnLeft,  Right -> Up
          | TurnRight, Up    -> Right
          | TurnRight, Right -> Down
          | TurnRight, Down  -> Left
          | TurnRight, Left  -> Up
      let nextTurn = 
        if tile <> Some Intersection then cart.NextTurn
        else
          match cart.NextTurn with
          | TurnLeft -> GoStraight
          | GoStraight -> TurnRight
          | TurnRight -> TurnLeft
      let collided = carts |> Map.containsKey (x', y')
      carts |> (Map.remove (x, y) >> Map.add (x', y') ({ Direction = direction; NextTurn = nextTurn; Collided = collided }))
  carts |> (Map.toSeq >> Seq.sortBy fst >> Seq.fold moveCart carts)

let findFirstCollision { Tiles = tiles; Carts = carts } =
  let rec loop carts =
    let carts = moveTick { Tiles = tiles; Carts = carts }
    carts |> (Map.toSeq >> Seq.sortBy fst >> Seq.tryFind (fun (_, cart) -> cart.Collided) >> Option.defaultWith (fun () -> loop carts))
  loop carts |> fst

let findLastCollision { Tiles = tiles; Carts = carts } =
  let mutable carts = carts
  while carts |> Map.count > 1 do
    carts <- moveTick { Tiles = tiles; Carts = carts } |> (Map.filter (fun _ cart -> not cart.Collided))
  carts |> (Map.toSeq >> Seq.head >> fst)

let testData = [|
  @"/->-\        "
  @"|   |  /----\"
  @"| /-+--+-\  |"
  @"| | |  | v  |"
  @"\-+-/  \-+--/"
  @"  \------/   "
|]

[<Fact>]
let ``First Collision - Test`` () =
  let actual = testData |> (parseInput >> findFirstCollision)
  let expected = (3, 7)
  Assert.Equal (expected, actual)

[<Fact>]
let ``First Collision - Actual`` () =
  let actual = File.ReadAllLines "Day13input.txt" |> (parseInput >> findFirstCollision)
  let expected = (104, 57)
  Assert.Equal (expected, actual)

[<Fact>]
let ``Last Collision - Actual`` () =
  let actual = File.ReadAllLines "Day13input.txt" |> (parseInput >> findLastCollision)
  let expected = (74, 67)
  Assert.Equal (expected, actual)