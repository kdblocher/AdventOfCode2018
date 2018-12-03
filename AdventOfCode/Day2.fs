module Day2
open System.Collections.Generic
open System.IO
open System.Text
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

// There should be some sort of bubble sort that is more efficient,
// but because the data size is small, I'm writing it the naive way.
let commonLetters (a : string, b : string) =
  let reify offIndex =
    match offIndex with
    | None -> Some a
    | Some i -> Some (a.Substring(0, i) + a.Substring(i + 1))
  let rec f index offIndex =
    if a.Length = index then
      reify offIndex
    else if a.[index] = b.[index] then
      f (index + 1) offIndex
    else if Option.isNone offIndex then
      f (index + 1) (Some index)
    else
      None
  f 0 None

let permute input =
  let cached = Seq.cache input
  Seq.allPairs cached cached |> Seq.where (fun (a, b) -> a <> b)

let findPair = permute >> Seq.map commonLetters >> Seq.tryPick id

[<Fact>]
let ``Common Letters Test`` () =
  let input = Seq.ofList [
    "abcde"
    "fghij"
    "klmno"
    "pqrst"
    "fguij"
    "axcye"
    "wvxyz"
  ]
  let actual = findPair input
  Assert.Equal (Some "fgij", actual)

[<Fact>]
let ``Common Letters Actual`` () =
  let input = File.ReadAllLines "Day2input.txt"
  let actual = findPair input
  Assert.Equal (Some "umdryabviapkozistwcnihjqx", actual)