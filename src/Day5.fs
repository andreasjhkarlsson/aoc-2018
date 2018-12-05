module Day5

open Common
open System

[<Day(5, "Alchemical Reduction")>]
let solve input =

    let polymer = input |> String.trim |> Seq.toList

    let reacts a b = (abs ((int a) - (int b))) = 32 // 32 = distance between uppercase/lowercase in ASCII

    let reduceOnce units =
        List.foldBack (fun u polymer -> 
            match polymer with
            | head::rest when reacts head u ->  rest
            | _ -> u::polymer
        ) units []

    let rec reduce units =
        let once = reduceOnce units
        let twice = reduceOnce once
        if once = twice then once
        else reduce twice
    
    let reduced = reduce polymer
        
    {
        Part1 =
            polymer
            |> reduce
            |> List.length
        Part2 =         
            polymer
            |> List.map Char.ToUpper
            |> List.distinct
            |> List.map (fun pType ->
                polymer
                |> List.filter (fun unit -> (Char.ToUpper unit) <> pType)
                |> reduce
            )
            |> List.map List.length
            |> List.min
    }