module Day4

open Common
open System

type Event = StartShift of int | FallsAsleep | WakesUp

type Observation = Event * DateTime

type Shift = {
    Guard: int
    SleepCycles: (DateTime * DateTime) list
}
    with
        static member GuardedBy {Guard = guard} = guard
        static member SleepsAt {SleepCycles = cycles} = cycles
        static member WasSleeping minute {SleepCycles = cycles} =
            cycles |> List.exists (fun (start, stop) ->
                minute >= start.Minute && minute < stop.Minute
            )

let rec parseSleepCycles observations =
    match observations with
    | (FallsAsleep, start)::(WakesUp, stop)::rest ->
        let sleepCycles, rest = parseSleepCycles rest
        ((start, stop) :: sleepCycles), rest
    | rest -> [], rest        

let rec parseShifts observations =
    match observations with
    | ((StartShift guard), datetime)::rest ->
        let sleepCycles, rest = parseSleepCycles rest
        let shifts = parseShifts rest
        { Guard = guard; SleepCycles = sleepCycles } :: shifts
    | [] -> []
    | _ -> failwith "parse error"

[<Day(4, "Repose Record")>]
let solve input =

    let schedule =
        input
        |> parseLinesAs (
            function
            | Regex @"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.+)" [year;month;day;hour;minute;rest] ->
                match rest with
                | "falls asleep" -> FallsAsleep
                | "wakes up" -> WakesUp
                | Regex @"Guard #(\d+) begins shift" [guard] -> StartShift (int guard)
                | _ -> failwith "parse error"
                , DateTime(int year, int month, int day, int hour, int minute, 0)
            | _ -> failwith "parse error"
        )
        |> Seq.sortBy snd
        |> Seq.toList
        |> parseShifts
        |> List.groupBy Shift.GuardedBy

    // Most sleepy is the guard who sleeps the most
    let mostSleepy, mostSleepyShifts =
        schedule
        |> List.sortByDescending (fun (_, shifts) ->
            shifts
            |> List.map Shift.SleepsAt
            |> List.concat
            |> List.sumBy (fun (start,stop) -> (stop-start).Minutes)
        )
        |> List.head
    
    // Which minute is he most often asleep?
    let mostSleepyMinuteForMostSleepy =
        [0..59]
        |> List.sortByDescending (fun minute ->
            mostSleepyShifts |> List.filter (Shift.WasSleeping minute) |> List.length
        )
        |> List.head

    // Which guard is most often asleep at one minute and what minute is that?
    let (minuteMostSlept, (guardWithMostCommonSleepMinute,_)) =
        [0..59] |*| schedule // Cartesian product (all minutes for all guards)
        |> Seq.sortByDescending (fun (minute, (_, shifts)) ->
            shifts |> List.filter (Shift.WasSleeping minute) |> List.length
        )
        |> Seq.head

    {
        Part1 = mostSleepy * mostSleepyMinuteForMostSleepy
        Part2 = minuteMostSlept * guardWithMostCommonSleepMinute
    }