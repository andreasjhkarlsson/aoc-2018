module Day5

open Common
open System
open System.Threading

type Async with
    static member StartWithStackSize (fn: unit -> 'a) stackSize =
        async {
            
            let result = ref Option<'a>.None
            
            let waitHandle = new ManualResetEvent(false)

            let thread = Thread((fun () ->
                result := Some (fn ())
                ignore <| waitHandle.Set()
            ), stackSize)

            thread.Start()
            
            let! waitResult = Async.AwaitWaitHandle waitHandle

            return (!result).Value

        }

[<Day(5, "Alchemical Reduction")>]
let solve input =

    let units = input |> String.trim |> Seq.toList

    let reacts a b =
        let diff = (int a) - (int b) 
        (diff*diff) = 32*32

    let rec reduceOnce units =
        match units with
        | first::second::rest ->
            if (reacts first second) then reduceOnce rest
            else first :: (reduceOnce (second :: rest))
        | _ -> units

    let rec reduce units =
        let once = reduceOnce units
        let twice = reduceOnce once
        if once = twice then once
        else reduce twice
        
    let STACK_SIZE = 2 * 1024 * 1024
    
    let reduced =
        Async.StartWithStackSize (fun () -> reduce units) STACK_SIZE
        |> Async.RunSynchronously
        
    let bestReduce =
        async {
        
            let pTypes = units |> List.map Char.ToUpper |> List.distinct

            let! results =
                pTypes
                |> List.map (fun pType -> units |> List.filter (fun u -> (Char.ToUpper u) <> pType))
                |> List.map (fun polymer -> async {
                    return! Async.StartWithStackSize (fun () -> reduce polymer) STACK_SIZE
                })
                |> Async.Parallel
            
            return
                results
                |> Array.map List.length
                |> Array.min

        } |> Async.RunSynchronously
        
    {
        Part1 = List.length reduced
        Part2 = bestReduce
    }