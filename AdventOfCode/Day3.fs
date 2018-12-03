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
  Area : Rectangle
}

let parseRegex = Regex ("#(\d+) @ (\d+),(\d+): (\d+)x(\d+)", RegexOptions.Singleline)
let parseInputLine line =
  let groups = (parseRegex.Match line).Groups
  let getValue index = groups.[index + 1].Value |> int
  {
    Number = getValue 0
    Area = {
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
  [| { Number = 1; Area = { Left = 1; Top = 3; Width = 4; Height = 4 } }; overlapInputData.[0] |]
  [| { Number = 2; Area = { Left = 3; Top = 1; Width = 4; Height = 4 } }; overlapInputData.[1] |]
  [| { Number = 3; Area = { Left = 5; Top = 5; Width = 2; Height = 2 } }; overlapInputData.[2] |]
|]

[<Theory>]
[<MemberData("overlapTestData")>]
let ``Overlap Tests - Parse`` claim input =
  let actual = parseInputLine input
  Assert.Equal (claim, actual)

type Intersection = {
  OverlappingArea : int
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
      OverlappingArea = intersection.Area
      RemainingArea = r1.Area + r2.Area - (2 * intersection.Area)
    }
    else None

[<Fact>]
let ``Overlap Tests - Find`` () =
  let getRects =
    let getRect = parseInputLine >> (fun c -> c.Area)
    Helpers.permute >> Helpers.removeDupes >> Seq.map (fun (a, b) -> getRect a, getRect b)
  let find = getRects >> Seq.map findOverlap >> Seq.choose id >> Seq.exactlyOne
  let actual = find overlapInputData
  let expected = {
    OverlappingArea = 4
    RemainingArea = 24
  }
  Assert.Equal (expected, actual)

//let ``Overlap Tests - Double Counting`` () =