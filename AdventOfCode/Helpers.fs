module Helpers
open System.Collections.Generic

let inline permute (input : 'a) =
  let cached = Seq.cache input
  let tupleNotEqual (a, b) = a <> b
  Seq.allPairs cached cached |> Seq.where tupleNotEqual

let inline removeDupes (pairs : ('a * 'a) seq) =
  let seen = HashSet<'a * 'a>()
  let pairNotSeen (a, b) = seen.Add((a, b)) && seen.Add((b, a))
  pairs |> Seq.where pairNotSeen

let inline curry f a b = f (a, b)
let inline uncurry f (a, b) = f a b

module LinkedList =
  let private move =
    let rec loop ring f i m = if i = 0 then m else loop ring f (i - 1) (f m)
    let result move1 cycle ring = loop ring (move1 >> Option.ofObj >> Option.defaultWith (cycle ring))
    result

  let clockwise ring =
    let next (m : LinkedListNode<_>) = m.Next
    let cycleNext (ring : LinkedList<_>) () = ring.First
    move next cycleNext ring
  let counterclockwise ring =
    let prev (m : LinkedListNode<_>) = m.Previous
    let cyclePrev (ring : LinkedList<_>) () = ring.Last
    move prev cyclePrev ring