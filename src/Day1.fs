module Day1

open Common

[<Day(1, "Chronal Calibration")>]
let solve input =

    let deltas = input |> parseLinesAs int

    {
        Part1 = Seq.sum deltas
        Part2 =
            Seq.repeat deltas
            |> Seq.scan (fun (f, seen) d ->  f + d, seen |> Set.add f) (0, Set.empty) // Calculate intermediate frequency sums
            |> Seq.find (fun (f, seen) -> seen |> Set.contains f) // Find first frequency sum that is already in seen set
            |> fst
    }