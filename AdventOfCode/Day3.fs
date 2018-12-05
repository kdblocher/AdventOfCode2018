module Day3
open System.Collections.Generic
open System.IO
open System.Text
open Xunit
open System.Text.RegularExpressions
open System
open System.Diagnostics

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

let getOverlappingArea rects =
  let minWidth, maxWidth =
    (rects |> (Seq.map (fun r -> r.Left) >> Seq.min)),
    (rects |> (Seq.map (fun r -> r.Right) >> Seq.max))
  let minHeight, maxHeight =
    (rects |> (Seq.map (fun r -> r.Top) >> Seq.min)),
    (rects |> (Seq.map (fun r -> r.Bottom) >> Seq.max))
  let height = maxHeight - minHeight
  let width = maxWidth - minWidth
  let array = Array.init width (fun _ -> Array.zeroCreate<int> height)
  let update (r : Rectangle) =
    seq { 0 .. r.Width - 1 } |> Seq.map ((+) (r.Left - minWidth)) |> Seq.iter (fun i ->
      seq { 0 .. r.Height - 1 } |> Seq.map ((+) (r.Top - minHeight)) |> Seq.iter (fun j ->
        array.[i].[j] <- array.[i].[j] + 1))
  rects |> Seq.iter update
  let sum1 e = if e > 1 then 1 else 0
  array |> Seq.sumBy (Seq.sumBy sum1)

let getTotals = Seq.map parseInputLine >> Seq.map (fun r -> r.Envelope) >> getOverlappingArea

[<Fact>]
let ``Overlap Tests - Overlapping Area`` () =
  let actual = getTotals overlapInputData
  let expected = 4
  Assert.Equal (expected, actual)

[<Fact>]
let ``Overlap Tests - Actual`` () =
  let actual = File.ReadAllLines "Day3input.txt" |> getTotals
  let expected = 124850
  Assert.Equal (expected, actual)