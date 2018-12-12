module Day12

open Common

type Pot = Empty | Plant
    with static member FromChar = function '.' -> Empty | '#' -> Plant | _ -> failwith "Invalid pot" 

type Rule = Rule of (Pot*Pot*Pot*Pot*Pot)*Pot
    with
        static member Pattern (Rule (pattern, _)) = pattern
        static member Result (Rule (_, result)) = result

[<Day(12, "Subterranean Sustainability")>]
let solve input =

    let lines = input |> parseLines

    let potLine =
        Seq.head lines
        |> function Regex @"initial state: ([.#]+)" [ps] -> ps |> Seq.map Pot.FromChar | _ -> failwith "Invalid input"
        |> Seq.indexed
        |> Map.ofSeq

    let rules =
        lines
        |> Seq.skip 2
        |> Seq.map (
            function
            | Regex @"([.#]+) => ([.#])" [pattern; result] ->
                let l1::l2::c::r1::r2::_ = (Seq.toList pattern |> List.map Pot.FromChar)
                Rule ((l1,l2,c,r1,r2), Pot.FromChar (Seq.head result))
            | _-> failwith "Invalid input"
        )
        |> Seq.toList
    
    let sumIndexes potLine =
        potLine
        |> Map.toList
        |> List.filter (snd >> ((=) Pot.Plant))
        |> List.sumBy fst

    let advance rules potLine =
        let get index = potLine |> Map.tryFind index |> Option.defaultValue Empty

        let min = (potLine |> Map.toList |> List.minBy fst |> fst) - 2
        let max = (potLine |> Map.toList |> List.maxBy fst |> fst) + 2 

        [min..max] 
        |> List.map (fun i ->
            i,
            let (_,_,pot,_,_) as line = get (i-2), get (i-1), get i, get (i+1), get (i+2)
            rules
            |> List.tryFind (Rule.Pattern >> ((=) line))
            |> Option.map Rule.Result
            |> Option.defaultValue pot
        )
        |> Map.ofList

    {
        Part1 = 
            [1..20]
            |> List.fold (fun pl _ -> advance rules pl) potLine 
            |> sumIndexes
        Part2 = 
            {1UL..50000000000UL}
            |> Seq.scan (fun pl _ -> advance rules pl) potLine
            |> Seq.windowed 3 
            |> Seq.indexed
            |> Seq.find (fun (i, w) ->
                let s1, s2, s3 = sumIndexes w.[0], sumIndexes w.[1], sumIndexes w.[2]
                (s2-s1) = (s3-s2)
            )
            |> (fun (i, w)->
                let diff = (sumIndexes w.[1]) - (sumIndexes w.[0]) |> uint64
                diff * (50000000000UL / ((uint64 i) + 1UL))
            )
    }
    