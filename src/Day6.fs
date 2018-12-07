module Day6

open Common

type Bounds = {Left: int; Right: int; Top: int; Bottom: int}

[<Day(6, "Chronal Coordinates")>]
let solve input =

    let coords =
        input
        |> parseLinesAs (String.split ", ")
        |> Seq.map (function | x::y::_ -> int x, int y | _ -> failwith "Invalid input")
        |> Seq.toList

    let manhattan (x1,y1) (x2, y2) = (abs (x1-x2)) + (abs (y1-y2))

    let bounds = {
        Left = coords |> List.minBy fst |> fst
        Right = coords |> List.maxBy fst |> fst
        Top = coords |> List.minBy snd |> snd
        Bottom = coords |> List.maxBy snd |> snd 
    }
        
    {
        Part1 = 
            [bounds.Left..bounds.Right] |*| [bounds.Top..bounds.Bottom]
            |> Seq.fold (fun map p ->
                let sorted =
                    coords
                    |> List.map (fun c -> c, manhattan c p)
                    |> List.sortBy snd
                in
                    sorted
                    |> List.filter (fun (_, d) -> d = (snd <| List.head sorted))
                    |> List.map fst
                |> function
                | [unique] -> 
                    match map |> Map.tryFind unique with
                    | Some lst -> map |> Map.add unique (p::lst)
                    | None -> map |> Map.add unique [p]
                | _ -> map
            ) Map.empty
            |> Map.toList
            |> List.map snd
            |> List.filter (fun points -> 
                points
                |> List.exists (fun (x,y) -> x = bounds.Left || x = bounds.Right || y = bounds.Top || y = bounds.Bottom)
                |> not
            )
            |> List.map List.length
            |> List.max
        Part2 = 
            [bounds.Left..bounds.Right] |*| [bounds.Top..bounds.Bottom]
            |> Seq.filter (fun p ->
                (coords |> List.map (manhattan p) |> List.sum) < 10000
            )
            |> Seq.length
    }