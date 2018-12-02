module Day2

open Common

[<Day(2, "Inventory Management System")>]
let solve input =

    let ids = input |> parseLines

    let checksum = 
        let hasExact occurences =  Seq.count >> Seq.map snd >> Seq.contains occurences
        let countExacts occurences = ids |> Seq.filter (hasExact occurences) |> Seq.length
        (countExacts 2) * (countExacts 3)


    let id =
        let closeness id1 id2 =
            Seq.zip id1 id2
            |> Seq.filter ((<||) (=))
            |> Seq.length

        ids
        |> Seq.choose (fun id -> // Choose strings that only diff 1 chars from eachother
            ids
            |> Seq.tryFind (closeness id >> ((=) (id.Length - 1)))
            |> Option.map (fun m -> id, m) // map pair
        )
        |> Seq.head // Select the first match
        ||> Seq.zip // Zip the strings together
        |> Seq.choose (fun (c1,c2) -> if c1 = c2 then Some (string c1) else None) // Keep matching chars
        |> String.concat ""

    {
        Part1 = checksum
        Part2 = id
    }