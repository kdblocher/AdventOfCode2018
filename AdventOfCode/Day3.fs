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

type Overlap = {
  Intervals : Interval list
} with
  member this.TotalInterval = {
    Start = this.Intervals |> List.map (fun l -> l.Start) |> List.min
    End   = this.Intervals |> List.map (fun l -> l.End) |> List.max
  }

let inline (><?) s1 s2 = s1.Start < s2.End || s1.End > s2.Start
let inline (>-<) s1 s2 = if s1 ><? s2 then Some { Start = s1.Start; End = s2.End } else None
let inline (<->) s1 s2 =
  if s1.Start < s2.Start && s1.End < s2.End then [ { Start = s1.Start; End = s2.Start }; { Start = s2.End; End = s1.End } ]
  else if s1.Start < s2.Start then [ { Start = s1.Start; End = s2.Start }]
  else [ { Start = s2.End; End = s2.End }]
let inline (<) s1 s2 = s1.End < s2.Start
let getYInterval r = { Start = r.Top; End = r.Bottom }
let getArea = (*) << List.sumBy (fun ys -> ys.End - ys.Start) << List.map (fun (o : Overlap) -> o.TotalInterval)

let rec driver update zero i (os : Overlap list) =
  match os with
  | [] -> zero ()
  | o :: os when i ><? o.TotalInterval -> { Intervals = update i o.Intervals } :: os
  | o :: os -> o :: driver update zero i os

let rec insert i =
  let rec f i ys =
    match ys with
    | [] -> [i]
    | y :: ys when i < y -> i :: y :: ys
    | y :: ys -> y :: f i ys
  driver f (fun () -> [{ Intervals = [i] }]) i

let rec remove =
  let rec f i ys =
    match ys with
    | [] -> []
    | y :: ys when i = y -> ys
    | y :: ys -> y :: f i ys
  driver f (fun () -> failwith "Not supported")


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