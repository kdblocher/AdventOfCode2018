module Day10

open System.Text
open System.Text.RegularExpressions
open Xunit
open System.IO

type Point = {
  X : int
  Y : int
}
type PointVector = {
  Position : Point
  Velocity : Point
}

let (*) sec pv = {
  X = pv.Position.X + pv.Velocity.X * sec
  Y = pv.Position.Y + pv.Velocity.Y * sec
}
let x p = p.X
let y p = p.Y
let agg d f = Seq.map d >> f
let minX = agg x Seq.min
let minY = agg y Seq.min
let maxX = agg x Seq.max
let maxY = agg y Seq.max
let diffX ps = maxX ps - minX ps
let diffY ps = maxY ps - minY ps

let regex = Regex "position=\<[ ]*([^,>]+),[ ]*([^,>]+)\> velocity=\<[ ]*([^,>]+),[ ]*([^,>]+)\>"
let parseInputLine input =
  let m = regex.Match input
  let getValue i = m.Groups.[i + 1].Value |> int
  {
    Position = { X = getValue 0; Y = getValue 1 }
    Velocity = { X = getValue 2; Y = getValue 3 }
  }

let hiTestData = [|
  "position=< 9,  1> velocity=< 0,  2>"
  "position=< 7,  0> velocity=<-1,  0>"
  "position=< 3, -2> velocity=<-1,  1>"
  "position=< 6, 10> velocity=<-2, -1>"
  "position=< 2, -4> velocity=< 2,  2>"
  "position=<-6, 10> velocity=< 2, -2>"
  "position=< 1,  8> velocity=< 1, -1>"
  "position=< 1,  7> velocity=< 1,  0>"
  "position=<-3, 11> velocity=< 1, -2>"
  "position=< 7,  6> velocity=<-1, -1>"
  "position=<-2,  3> velocity=< 1,  0>"
  "position=<-4,  3> velocity=< 2,  0>"
  "position=<10, -3> velocity=<-1,  1>"
  "position=< 5, 11> velocity=< 1, -2>"
  "position=< 4,  7> velocity=< 0, -1>"
  "position=< 8, -2> velocity=< 0,  1>"
  "position=<15,  0> velocity=<-2,  0>"
  "position=< 1,  6> velocity=< 1,  0>"
  "position=< 8,  9> velocity=< 0, -1>"
  "position=< 3,  3> velocity=<-1,  1>"
  "position=< 0,  5> velocity=< 0, -1>"
  "position=<-2,  2> velocity=< 2,  0>"
  "position=< 5, -2> velocity=< 1,  2>"
  "position=< 1,  4> velocity=< 2,  1>"
  "position=<-2,  7> velocity=< 2, -2>"
  "position=< 3,  6> velocity=<-1, -1>"
  "position=< 5,  0> velocity=< 1,  0>"
  "position=<-6,  0> velocity=< 2,  0>"
  "position=< 5,  9> velocity=< 1, -2>"
  "position=<14,  7> velocity=<-2,  0>"
  "position=<-3,  6> velocity=< 2, -1>"
|]

let align height pvs =
  let render ps =
    let mx = minX ps
    let my = minY ps
    let ps = ps |> Seq.map (fun p -> { X = p.X - mx; Y = p.Y - my }) |> Seq.cache
    let array = Array.init (diffY ps + 1) (fun _ -> (Array.replicate (diffX ps + 1) '.'))
    let set p = array.[p.Y].[p.X] <- '#'
    ps |> Seq.iter set
    let sb = StringBuilder ()
    let appendLine _ = sb.AppendLine() |> ignore
    array |> Array.iter (Array.iter (sb.Append >> ignore) >> appendLine)
    sb.ToString()

  let rec delta i =
    let ps = pvs |> Seq.map ((*) i) |> Seq.cache
    maxY ps - minY ps, ps

  let rec guess i =
    let dy, ps = delta i
    if dy <= height then
      (i, render ps)
    else
      guess (i + 1)

  let d0, _ = delta 0
  let d1, _ = delta 1
  guess ((height - d0) / (d1 - d0))

let rasterOutput output = (output |> List.reduce (fun s1 s2 -> s1 + "\r\n" + s2)) + "\r\n"

[<Fact>]
let ``Stars Align - Test`` () =
  let (_, actual) = hiTestData |> (Seq.map parseInputLine >> align 8)
  let output = [
    "#...#..###"
    "#...#...#."
    "#...#...#."
    "#####...#."
    "#...#...#."
    "#...#...#."
    "#...#...#."
    "#...#..###"
  ]
  let expected = rasterOutput output
  Assert.Equal (expected, actual)

[<Fact>]
let ``Stars Align - Actual`` () =
  let actual = File.ReadAllLines "Day10input.txt" |> (Seq.map parseInputLine >> align 9)
  let output = [
    "######..######...####...#####....####....####....####......###"
    ".....#.......#..#....#..#....#..#....#..#....#..#....#......#."
    ".....#.......#..#.......#....#..#.......#.......#...........#."
    "....#.......#...#.......#....#..#.......#.......#...........#."
    "...#.......#....#.......#####...#.......#.......#...........#."
    "..#.......#.....#.......#....#..#..###..#..###..#...........#."
    ".#.......#......#.......#....#..#....#..#....#..#...........#."
    "#.......#.......#.......#....#..#....#..#....#..#.......#...#."
    "#.......#.......#....#..#....#..#...##..#...##..#....#..#...#."
    "######..######...####...#####....###.#...###.#...####....###.."
  ]
  let expected = 10886, rasterOutput output
  Assert.Equal (expected, actual)

  