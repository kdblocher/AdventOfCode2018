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