module Day3

open Common

type Claim = {
    Id: int
    Left: int
    Top: int
    Width: int
    Height: int
} with
    static member parse = 
        function // #1 @ 1,2: 20x3
        | Regex @"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" [id;l;t;w;h] -> { Id = int id; Left = int l; Top = int t; Width = int w; Height = int h }
        | _ -> failwith "Parse error"
    static member Squares (claim: Claim) = seq {
        for x in claim.Left..(claim.Left+claim.Width-1) do
            for y in claim.Top..(claim.Top+claim.Height-1) do
                yield x, y
    }
    
[<Day(3, "No Matter How You Slice It")>]
let solve input =

    let claims = input |> parseLinesAs Claim.parse
    
    // Build lookup map
    let claimedBy =
        Seq.fold (fun claimedBy claim -> 
            claim
            |> Claim.Squares
            |> Seq.fold (fun claimed si ->
                // Claimed by someone before? 
                match claimed |> Map.tryFind si with
                | Some claims -> claimed |> Map.add si (claim :: claims) // Add to list of previous claims
                | None -> claimed |> Map.add si [claim] // Add as first claim
            ) claimedBy
        ) Map.empty<int*int,Claim list> claims

    let claimedByMultiple = claimedBy |> Map.filter (fun _ -> function | _::_::_ -> true | _ -> false) 

    let nonOverlapped =
        claims 
        |> Seq.find (fun claim ->
            Claim.Squares claim
            |> Seq.forall (fun c -> 
                match claimedBy |> Map.tryFind c with
                | Some [c] -> c = claim
                | _ -> false
            )
        )

    {
        Part1 = claimedByMultiple.Count
        Part2 = nonOverlapped.Id
    }