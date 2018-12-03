module Day2
open System.Collections.Generic
open System.IO
open Xunit

let findRepeats (input : string) =
  let counts = Array.replicate 26 0
  let f (c : char) =
    counts.[int c - int 'a'] <- counts.[int c - int 'a'] + 1
  input |> Seq.iter f
  counts |> Seq.exists ((=) 2),
  counts |> Seq.exists ((=) 3)

let bool2int b = if b then 1 else 0
let checksum ls =
  ls
  |> Seq.map (fun (a, b) -> (bool2int a, bool2int b))
  |> Seq.reduce (fun (a, b) (c, d) -> a + c, b + d)
  |> (fun (a, b) -> a * b)



let checksumTestData : obj [] [] = [|
  [| false; false; "abcdef" |]
  [| true ; true ; "bababc" |]
  [| true ; false; "abbcde" |]
  [| false; true ; "abcccd" |]
  [| true ; false; "aabcdd" |]
  [| true ; false; "abcdee" |]
  [| false; true ; "ababab" |]
|]

[<Theory>]
[<MemberData("checksumTestData")>]
let ``Checksum Tests - Find Repeats`` hasTwo hasThree input =
  let actual = findRepeats input
  Assert.Equal ((hasTwo, hasThree), actual)

[<Fact>]
let ``Checksum Tests - Checksum`` () =
  let actual =
    checksumTestData
    |> Seq.map (fun x -> x.[2].ToString())
    |> Seq.map findRepeats
    |> checksum
  Assert.Equal (12, actual)

[<Fact>]
let ``Checksum Actual`` () =
  let input = File.ReadAllLines "Day2input.txt"
  let actual = input |> Seq.map findRepeats |> checksum
  Assert.Equal (5704, actual)