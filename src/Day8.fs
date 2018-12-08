module Day8

open Common
open System

type Node = Node of (int list) * (Node list) with
    static member Children (Node (_, children)) = children
    static member Entries (Node (entries, _)) = entries
    static member Child index = Node.Children >> List.indexed >> List.tryFind (fst >> ((=) index)) >> Option.map snd

[<Day(8, "Memory Maneuver")>]
let solve (input: string) =
    
    let input = input |> String.trim |> String.split " " |> List.map int

    let rec buildNode input: (Node* int list) =
        
        let expect fn times input =
                Seq.fold (fun (result, input) _ ->
                    let e, rest = fn input
                    e::result, rest
                ) ([], input) [1..times]        
        
        let entry input =
            match input with
            | head::rest -> head, rest
            | _ -> failwith "Invalid input"

        match input with
        | childCount::entryCount::rest ->
            let children, rest = expect buildNode childCount rest
            let entries, rest = expect entry entryCount rest
            (Node (List.rev entries, List.rev children)), rest
        | _ -> failwith "Invalid input"
        
    let tree, _ = buildNode input

    {
        Part1 =
            let rec checksum (Node (entries, children)) =
                entries
                |> List.sum
                |> (+) (children |> List.sumBy checksum)
            in checksum tree            
        Part2 = 
            let rec checksum (Node (entries, children) as node) =
                if children = [] then
                    List.sum entries
                else
                    entries
                    |> List.choose (fun e -> Node.Child (e-1) node)
                    |> List.sumBy checksum
            in checksum tree
    }