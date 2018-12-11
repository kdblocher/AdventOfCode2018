module Day11

open Xunit

type Cell = {
  RackID : int
  Power : int
}

let hundredsDigit x = (x % 1000 - x % 100) / 100
let createCell serial x y =
  let rackID = x + 10
  let power = hundredsDigit ((rackID * y + serial) * rackID) - 5
  { RackID = rackID; Power = power }
 
let cartesian xs ys = 
  xs |> Seq.collect (fun x -> ys |> Seq.map (fun y -> x, y))
 
let maxPower x y minSize maxSize serial =
  let array = Array.init x (fun i -> Array.init y (createCell serial i))
  let powerLevelOfCell size (x, y) =
    Array.sub array x size |> Array.sumBy (fun i -> Array.sub i y size |> Array.sumBy (fun j -> j.Power))
  let findMax size =
    let max = cartesian (seq { 0 .. (x - size) }) (seq { 0 .. (y - size) }) |> Seq.maxBy (powerLevelOfCell size)
    size, max
  seq { minSize .. maxSize } |> (Seq.map findMax >> Seq.maxBy snd)

let defaultMaxPower = maxPower 300 300 3 3 >> snd

[<Fact>]
let ``Max Power - Test`` () =
  let actual = defaultMaxPower 18
  let expected = 33, 45
  Assert.Equal (expected, actual)

[<Fact>]
let ``Max Power - Test 2`` () =
  let actual = defaultMaxPower 42
  let expected = 21, 61
  Assert.Equal (expected, actual)
  
[<Fact>]
let ``Max Power - Actual`` () =
  let actual = defaultMaxPower 8199
  let expected = 235, 87
  Assert.Equal (expected, actual)

[<Fact>]
let ``Max Power Any Size - Actual`` () =
  let (size, actual) = maxPower 300 300 1 300 8199
  let expected = 235, 87
  Assert.Equal (expected, actual)