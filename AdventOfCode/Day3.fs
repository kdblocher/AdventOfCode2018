module Day3
open System.Collections.Generic
open System.IO
open System.Text
open Xunit
open System.Text.RegularExpressions

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
  Rectangle : Rectangle
}

let parseRegex = Regex ("#(\d+) @ (\d+),(\d+): (\d+)x(\d+)", RegexOptions.Singleline)
let parseInputLine line =
  let groups = (parseRegex.Match line).Groups
  let getValue index = groups.[index + 1].Value |> int
  {
    Number = getValue 0
    Rectangle = {
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
  [| { Number = 1; Rectangle = { Left = 1; Top = 3; Width = 4; Height = 4 } }; overlapInputData.[0] |]
  [| { Number = 2; Rectangle = { Left = 3; Top = 1; Width = 4; Height = 4 } }; overlapInputData.[1] |]
  [| { Number = 3; Rectangle = { Left = 5; Top = 5; Width = 2; Height = 2 } }; overlapInputData.[2] |]
|]

[<Theory>]
[<MemberData("overlapTestData")>]
let ``Overlap Tests - Parse`` claim input =
  let actual = parseInputLine input
  Assert.Equal (claim, actual)

type Intersection = {
  Rectangle : Rectangle
  RemainingArea : int
}

let findOverlap (r1 : Rectangle, r2 : Rectangle) =
  let left   = max r1.Left r2.Left
  let right  = min r1.Right r2.Right
  let top    = max r1.Top r2.Top
  let bottom = min r1.Bottom r2.Bottom
  if (left < right && top < bottom) then
    let intersection = {
      Left = left
      Top = top
      Width = right - left
      Height = bottom - top
    }
    Some {
      Rectangle = intersection
      RemainingArea = r1.Area + r2.Area - (2 * intersection.Area)
    }
    else None
 
let inline permute x = (Helpers.permute >> Helpers.removeDupes) x
let getRects =
  let getRect = parseInputLine >> (fun c -> c.Rectangle)
  permute >> Seq.map (fun (a, b) -> getRect a, getRect b)

let findIntersectionOverlaps =
  Seq.map findOverlap >> Seq.choose id
 
let findAllOverlaps =
  getRects >> findIntersectionOverlaps

let transformIntersections =
  let f (acc, rs) i = (acc + i.RemainingArea, Seq.append rs (Seq.singleton i.Rectangle))
  Seq.fold f (0, Seq.empty)

[<Fact>]
let ``Overlap Tests - Find`` () =
  let actual = findAllOverlaps overlapInputData |> Seq.exactlyOne
  let expected = {
    Rectangle = {
      Left = 3
      Top = 3
      Width = 2
      Height = 2
    }
    RemainingArea = 24
  }
  Assert.Equal (expected, actual)

let walkOverlaps input =
  let rec f acc rs =
    let (acc0, rs0) = rs |> (permute >> findIntersectionOverlaps >> transformIntersections)
    if Seq.isEmpty rs0
      then acc + (rs |> Seq.sumBy (fun r -> r.Area))
      else f (acc + acc0) rs0
  f 0 ((findAllOverlaps >> transformIntersections >> snd) input)
      
[<Fact>]
let ``Overlap Tests - Double Counting 1`` () =
  let actual = walkOverlaps overlapInputData
  Assert.Equal (4, actual)

[<Fact>]
let ``Overlap Tests - Double Counting 2`` () =
  let actual = walkOverlaps (overlapInputData |> Array.append (Array.singleton "#4 @ 2,2: 2x2"))
  Assert.Equal (7, actual)