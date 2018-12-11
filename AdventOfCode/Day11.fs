module Day11

open Xunit

let hundredsDigit x = (x % 1000 - x % 100) / 100
let createCell serial x y =
  let rackID = x + 10
  let power = hundredsDigit ((rackID * y + serial) * rackID) - 5
  power

type Result = {
  X : int
  Y : int
  Size : int
  Sum : int
}

let dp (array : _ [] []) offset (i, j) =
  let bx = i - offset < 0
  let by = j - offset < 0
  let dx = if bx then 0 else array.[i - offset].[j]
  let dy = if by then 0 else array.[i].[j - offset]
  let dd = if bx || by then 0 else array.[i - offset].[j - offset]
  dx + dy - dd

let maxPower x y minSize maxSize serial =
  
  let powers =
    Array.init x (fun i -> Array.init y (createCell serial i))

  let sums =
    let sums = Array.init x (fun i -> Array.zeroCreate<int> y)
    let dp = dp sums 1
    seq { 0 .. x - 1 } |> Seq.iter (fun i ->
      seq { 0 .. y - 1 } |> Seq.iter (fun j ->
        sums.[i].[j] <- powers.[i].[j] + dp (i, j)))
    sums
  
  let findMaxResult = Seq.maxBy (fun r -> r.Sum)
  let findMaxSquare size =
    let dp = dp sums size
    let allSizes =
      seq { size - 1 .. x - 1 } |> Seq.collect (fun i ->
        seq { size - 1 .. y - 1 } |> Seq.map (fun j -> {
          Size = size
          X = i - size + 1
          Y = j - size + 1
          Sum = sums.[i].[j] - dp (i, j)
        }))
    allSizes |> findMaxResult
  seq { minSize .. maxSize } |> (Seq.map findMaxSquare >> findMaxResult)

let defaultMaxPower = maxPower 300 300 3 3

[<Fact>]
let ``Max Power - Test`` () =
  let { X = x; Y = y } = defaultMaxPower 18
  let expected = 33, 45
  Assert.Equal (expected, (x, y))

[<Fact>]
let ``Max Power - Test 2`` () =
  let { X = x; Y = y } = defaultMaxPower 42
  let expected = 21, 61
  Assert.Equal (expected, (x, y))
  
[<Fact>]
let ``Max Power - Actual`` () =
  let { X = x; Y = y } = defaultMaxPower 8199
  let expected = 235, 87
  Assert.Equal (expected, (x, y))

[<Fact>]
let ``Max Power Any Size - Actual`` () =
  let { X = x; Y = y; Size = size } = maxPower 300 300 1 300 8199
  let expected = 234, 272, 18
  Assert.Equal (expected, (x, y, size))