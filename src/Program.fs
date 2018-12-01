open System
open Common

[<EntryPoint>]
let main argv = 
    
    printfn ">>> Advent of Code 2018 in F# >>>"

    let day =
        if argv.Length > 0 then int argv.[0]
        else printf "Day to solve: "; int <| Console.ReadLine ()

    
    match DayAttribute.All |> List.tryFind (fun (d,_) -> d.Day = day) with
    | Some (day, fn) ->

        printfn "\n*** Day %d \"%s\" ***" day.Day day.Title

        let input =
            if argv.Length > 1 then argv.[1]
            else printf "Input to puzzle: "; Console.ReadAll ()

        let solution = fn.Invoke(null, [|input|])
        printfn "\n*** Solution ***"
        printfn "Part 1: %A" <| solution.GetType().GetProperty("Part1").GetValue(solution)
        printfn "Part 2: %A" <| solution.GetType().GetProperty("Part2").GetValue(solution)

        0
    | None ->
        printfn "Error: This day has not been solved yet"
        -1