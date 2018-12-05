module Day3
open System.Collections.Generic
open System.IO
open System.Text
open Xunit
open System.Text.RegularExpressions
open System

type Rectangle = {
  Left : int
  Top : int
  Width : int
  Height : int
} with
  member this.Right = this.Left + this.Width
  member this.Bottom = this.Top + this.Height
  member this.Area = this.Width * this.Height

type Claim = {
  Number : int
  Envelope : Rectangle
}

let parseRegex = Regex ("#(\d+) @ (\d+),(\d+): (\d+)x(\d+)", RegexOptions.Singleline)
let parseInputLine line =
  let groups = (parseRegex.Match line).Groups
  let getValue index = groups.[index + 1].Value |> int
  {
    Number = getValue 0
    Envelope = {
      Left   = getValue 1
      Top    = getValue 2
      Width  = getValue 3
      Height = getValue 4
    }
  }

let overlapInputData = [|
  "#1 @ 1,3: 4x4"
  "#2 @ 3,1: 4x4"
  "#3 @ 5,5: 2x2"
|]
let overlapTestData : obj [] [] = [|
  [| { Number = 1; Envelope = { Left = 1; Top = 3; Width = 4; Height = 4 } }; overlapInputData.[0] |]
  [| { Number = 2; Envelope = { Left = 3; Top = 1; Width = 4; Height = 4 } }; overlapInputData.[1] |]
  [| { Number = 3; Envelope = { Left = 5; Top = 5; Width = 2; Height = 2 } }; overlapInputData.[2] |]
|]

[<Theory>]
[<MemberData("overlapTestData")>]
let ``Overlap Tests - Parse`` claim input =
  let actual = parseInputLine input
  Assert.Equal (claim, actual)

type Interval = {
  Start : int
  End : int
}
let inline (><?) s1 s2 = s1.Start <= s2.End && s1.Start <= s2.End
let inline (>-<) s1 s2 = if s1.Start < s2.Start then { Start = s1.Start; End = s2.End } else { Start = s2.Start; End = s1.End }
//let inline (<->) s1 s2 =
//  if s1.Start < s2.Start && s1.End < s2.End then [ { Start = s1.Start; End = s2.Start }; { Start = s2.End; End = s1.End } ]
//  else if s1.Start < s2.Start then [ { Start = s1.Start; End = s2.Start }]
//  else [ { Start = s2.End; End = s2.End }]
//let inline (<) s1 s2 = s1.End < s2.Start
let getYInterval r = { Start = r.Top; End = r.Bottom }

type OverlapTree =
| Leaf of Span : Interval
| Node of Merged : Interval * Left : OverlapTree * Right : OverlapTree
  with
    member this.Interval =
      match this with 
      | Leaf i -> i
      | Node (Merged = m) -> m
    static member Create ((l : OverlapTree), (r : OverlapTree)) = Node(l.Interval >-< r.Interval, l, r)

type Overlap = {
  Tree : OverlapTree
} with
  member this.TotalInterval =
    match this.Tree with
    | Leaf i -> i
    | Node(Merged = i) -> i

let getArea = (*) << List.sumBy (fun ys -> ys.End - ys.Start) << List.map (fun (o : Overlap) -> o.TotalInterval)

let rec insert i (os : Overlap list) =
  let rec f i tree =
    match tree with
    | Leaf l -> Node(i >-< l, Leaf i, Leaf l)
    | Node(m, l0, r0) ->
      let b = i < l0.Interval
      let l, r =
        (if b then Node(i >-< l0.Interval, Leaf i, l0) else l0),
        (if b then r0 else Node(i >-< r0.Interval, r0, Leaf i))
      OverlapTree.Create(l, r)
  match os with
  | [] -> [{ Tree = Leaf i }]
  | o :: os when i ><? o.TotalInterval -> { Tree = f i o.Tree } :: os
  | o :: os -> o :: insert i os

type RemoveResult =
| Success of OverlapTree option
| Failure of OverlapTree
let rec remove i (os : Overlap list) =
  let rec f i tree =
    match tree with
    | Leaf j when i = j -> Success None
    | Node (_, Leaf l, r) when l = i -> Success (Some r)
    | Node (_, l, Leaf r) when r = i -> Success (Some l)
    | Node (_, l, r) ->
      let remL = if i ><? l.Interval then f i l else Failure l
      let remR =
        match remL with
        | Failure _ -> if i ><? r.Interval then f i r else Failure r
        | _ -> Failure r
      let succ = OverlapTree.Create
      match remL, remR with
      | Failure l0, Failure r0 -> Failure (succ (l0, r0))
      | Success (Some l0), Failure r0 -> Success (Some (succ (l0, r0)))
      | Failure l0, Success (Some r0) -> Success (Some (succ (l0, r0)))
      | _ -> failwith "Shouldn't happen"
    | Leaf j -> Failure (Leaf j)
  match os with
  | [] -> failwith "Shouldn't happen"
  | o :: os when i ><? o.TotalInterval ->
    match f i o.Tree with
    | Success r ->
      match r with
      | Some t -> { Tree = o.Tree } :: os
      | None -> os
    | Failure _ -> failwith "Shouldn't happen"
  | o :: os -> o :: remove i os

type XEventType = Start of Rectangle | End of Rectangle
type XEvent = {
  X : int
  Event : XEventType
}

let sweep (rects : Rectangle seq) =
  let rects = rects |> List.ofSeq |> List.sortBy (fun r -> r.Right)
  let getAllXs =
    let dup r = [
      { X = r.Left; Event = Start r }
      { X = r.Right; Event = End r }]
    List.collect dup >> List.sortBy (fun x -> x.X)
  let rec f (xs : XEvent list) (ys : Overlap list) (x0 : int) (area : int) =
    match xs with
    | [] -> area
    | { X = x; Event = e } :: xs ->
      let area0 = getArea ys (x - x0)
      let update =
        match e with
        | Start r -> insert (getYInterval r)
        | End r -> remove (getYInterval r)
      f xs (update ys) x (area + area0)
  f (getAllXs rects) [] 0 0

[<Fact>]
let ``Overlap Tests - Find`` () =
  let actual = overlapInputData |> (Seq.map parseInputLine >> Seq.map (fun r -> r.Envelope) >> sweep)
  let expected = 32
  Assert.Equal (expected, actual)