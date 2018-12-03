module Day1
open System.Collections.Generic
open System.IO
open Xunit

let parseInput (input : string) =
  input.Split '\n' |> Seq.map int

let calibrate =
  parseInput >> Seq.sum

let calibrateTestData : obj [] [] = [|
  [| 3; "+1\n+1\n+1" |]
  [| 0; "+1\n+1\n-2" |]
  [| -6; "-1\n-2\n-3" |]
|]

[<Theory>]
[<MemberData("calibrateTestData")>]
let ``Calibrate Tests`` expected input =
  let actual = calibrate input
  Assert.Equal (expected, actual)

[<Fact>]
let ``Calibrate Actual`` () =
  let input = File.ReadAllText "Day1input.txt"
  let actual = calibrate input
  Assert.Equal (520, actual)

let infiniteInput input =
  let frequencies = parseInput input
  let rec f () = seq { yield! frequencies; yield! f () }
  f ()

let findRepeat (input : string) =
  let seen = HashSet<int> ()
  let items = (infiniteInput input).GetEnumerator()
  let mutable cur = 0
  while seen.Add cur && items.MoveNext() do
    cur <- cur + items.Current
  cur

let findRepeatTestData : obj [] [] = [|
  [| 0; "+1\n-1" |]
  [| 10; "+3\n+3\n+4\n-2\n-4" |]
  [| 5; "-6\n+3\n+8\n+5\n-6" |]
  [| 14; "+7\n+7\n-2\n-7\n-4" |]
|]

[<Theory>]
[<MemberData("findRepeatTestData")>]
let ``Find Repeat Tests`` expected input =
  let actual = findRepeat input
  Assert.Equal (expected, actual)

[<Fact>]
let ``Find Repeat Actual`` () =
  let input = File.ReadAllText "Day1input.txt"
  let actual = findRepeat input
  Assert.Equal (394, actual)