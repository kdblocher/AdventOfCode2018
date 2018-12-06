module Day4
open System.Text.RegularExpressions
open Xunit
open System

type GuardAction =
| Shift of int
| Sleep
| Wake
type GuardRecord = {
  Date : DateTime
  Action : GuardAction
}

let timeStampRegex = Regex ("\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2})\]", RegexOptions.Singleline)
let guardBeginRegex = Regex ("Guard #(\d+) begins shift", RegexOptions.Singleline)
let sleepRegex = Regex ("falls asleep", RegexOptions.Singleline)
let wakeRegex = Regex ("wakes up", RegexOptions.Singleline)
let parseInputLine line =
  let guard =
    let m = guardBeginRegex.Match line
    if m.Success then (Some << Shift << int) m.Groups.[1].Value else None
  let sleep =
    let m = sleepRegex.Match line
    if m.Success then Some Sleep else None
  let wake =
    let m = wakeRegex.Match line
    if m.Success then Some Wake else None
  let (<|>) = Option.orElse
  {
    Date = DateTime.Parse (timeStampRegex.Match line).Groups.[1].Value
    Action = (guard <|> sleep <|> wake) |> Option.defaultWith (fun () -> failwith "Bad input")
  }

let guardRecords = [|
  "[1518-11-01 00:00] Guard #10 begins shift"
  "[1518-11-01 00:05] falls asleep"
  "[1518-11-01 00:25] wakes up"
  "[1518-11-01 00:30] falls asleep"
  "[1518-11-01 00:55] wakes up"
  "[1518-11-01 23:58] Guard #99 begins shift"
  "[1518-11-02 00:40] falls asleep"
  "[1518-11-02 00:50] wakes up"
  "[1518-11-03 00:05] Guard #10 begins shift"
  "[1518-11-03 00:24] falls asleep"
  "[1518-11-03 00:29] wakes up"
  "[1518-11-04 00:02] Guard #99 begins shift"
  "[1518-11-04 00:36] falls asleep"
  "[1518-11-04 00:46] wakes up"
  "[1518-11-05 00:03] Guard #99 begins shift"
  "[1518-11-05 00:45] falls asleep"
  "[1518-11-05 00:55] wakes up"
|]

let guardParseTestData : obj [] [] = [|
  [| guardRecords.[0] ; { Date = DateTime(1518, 11, 01, 00, 00, 00); Action = Shift 10 } |]
  [| guardRecords.[1] ; { Date = DateTime(1518, 11, 01, 00, 05, 00); Action = Sleep } |]
  [| guardRecords.[2] ; { Date = DateTime(1518, 11, 01, 00, 25, 00); Action = Wake } |]
  [| guardRecords.[3] ; { Date = DateTime(1518, 11, 01, 00, 30, 00); Action = Sleep } |]
  [| guardRecords.[4] ; { Date = DateTime(1518, 11, 01, 00, 55, 00); Action = Wake } |]
  [| guardRecords.[5] ; { Date = DateTime(1518, 11, 01, 23, 58, 00); Action = Shift 99 } |]
  [| guardRecords.[6] ; { Date = DateTime(1518, 11, 02, 00, 40, 00); Action = Sleep } |]
  [| guardRecords.[7] ; { Date = DateTime(1518, 11, 02, 00, 50, 00); Action = Wake } |]
  [| guardRecords.[8] ; { Date = DateTime(1518, 11, 03, 00, 05, 00); Action = Shift 10 } |]
  [| guardRecords.[9] ; { Date = DateTime(1518, 11, 03, 00, 24, 00); Action = Sleep } |]
  [| guardRecords.[10]; { Date = DateTime(1518, 11, 03, 00, 29, 00); Action = Wake } |]
  [| guardRecords.[11]; { Date = DateTime(1518, 11, 04, 00, 02, 00); Action = Shift 99 } |]
  [| guardRecords.[12]; { Date = DateTime(1518, 11, 04, 00, 36, 00); Action = Sleep } |]
  [| guardRecords.[13]; { Date = DateTime(1518, 11, 04, 00, 46, 00); Action = Wake } |]
  [| guardRecords.[14]; { Date = DateTime(1518, 11, 05, 00, 03, 00); Action = Shift 99 } |]
  [| guardRecords.[15]; { Date = DateTime(1518, 11, 05, 00, 45, 00); Action = Sleep } |]
  [| guardRecords.[16]; { Date = DateTime(1518, 11, 05, 00, 55, 00); Action = Wake } |]
|]

[<Theory>]
[<MemberData("guardParseTestData")>]
let ``Guard Tests - Parse`` input record =
  let actual = parseInputLine input
  Assert.Equal (record, actual)

type GuardShift = {
  Date : DateTime
  SleepIntervals : (DateTime * TimeSpan) list
}
type Guard = {
  ID : int
  Shifts : Map<DateTime, GuardShift>
}

let makeDatabase (records : GuardRecord list) =
  let adjustShiftDate (date : DateTime) =
    if date.Hour > 0 then date.Date.AddDays 1. else date.Date
  let endShift db guard shift =
    let shift = { shift with SleepIntervals = shift.SleepIntervals |> List.rev }
    let shifts = guard.Shifts |> Map.add shift.Date shift
    let guard = { guard with Shifts = shifts }
    db |> Map.add guard.ID guard
  let rec startShift records db gid shiftDate =
    let guard = db |> Map.tryFind gid |> Option.defaultWith (fun () -> { ID = gid; Shifts = Map [] })
    loop records db guard { Date = shiftDate; SleepIntervals = [] } None
  and loop records db guard shift sleeping =
    match records with
    | [] ->
      endShift db guard shift
    | { Date = date; Action = a } :: records ->
      match a with
      | Shift gid ->
        match sleeping with 
        | Some _ -> failwith "sleeping at end of shift"
        | None ->
          startShift records (endShift db guard shift) gid (adjustShiftDate date)
      | Sleep ->
        match sleeping with
        | Some _ -> failwith "already asleep"
        | None -> loop records db guard shift (Some date)
      | Wake ->
        match sleeping with
        | Some d -> loop records db guard { shift with SleepIntervals = (d, date - d) :: shift.SleepIntervals } None
        | None -> failwith "already awake"
  let db = Map []
  match records |> List.sortBy (fun r -> r.Date) with
  | [] -> db
  | { Date = d; Action = Shift gid } :: records -> startShift records db gid (adjustShiftDate d)
  | _ -> failwith "Record set must start with a shift action"

(*
Date   ID   Minute
000000000011111111112222222222333333333344444444445555555555
012345678901234567890123456789012345678901234567890123456789
11-01  #10  .....####################.....#########################.....
11-02  #99  ........................................##########..........
11-03  #10  ........................#####...............................
11-04  #99  ....................................##########..............
11-05  #99  .............................................##########.....
*) 
let guardDatabaseTestData = Map [
  (10, {
    ID = 10
    Shifts = Map [
      (DateTime(1518, 11, 01), {
        Date = DateTime(1518, 11, 01)
        SleepIntervals = [
          DateTime(1518, 11, 01, 00, 05, 00), TimeSpan.FromMinutes(20.)
          DateTime(1518, 11, 01, 00, 30, 00), TimeSpan.FromMinutes(25.)
        ]
      })
      (DateTime(1518, 11, 03), {
        Date = DateTime(1518, 11, 03)
        SleepIntervals = [
          DateTime(1518, 11, 03, 00, 24, 00), TimeSpan.FromMinutes(5.)
        ]
      })
    ]
  })
  (99, {
    ID = 99
    Shifts = Map [
      (DateTime(1518, 11, 02), {
        Date = DateTime(1518, 11, 02)
        SleepIntervals = [
          DateTime(1518, 11, 02, 00, 40, 00), TimeSpan.FromMinutes(10.)
        ]
      })
      (DateTime(1518, 11, 04), {
        Date = DateTime(1518, 11, 04)
        SleepIntervals = [
          DateTime(1518, 11, 04, 00, 36, 00), TimeSpan.FromMinutes(10.)
        ]
      })
      (DateTime(1518, 11, 05), {
        Date = DateTime(1518, 11, 05)
        SleepIntervals = [
          DateTime(1518, 11, 05, 00, 45, 00), TimeSpan.FromMinutes(10.)
        ]
      })
    ]
  })
]

[<Fact>]
let ``Guard Tests - Data Conversion`` () =
  let actual = (List.ofArray >> List.map parseInputLine >> makeDatabase >> Map.toList) guardRecords
  let expected = guardDatabaseTestData |> Map.toList
  Assert.Equal<(int * Guard) list> (expected, actual)