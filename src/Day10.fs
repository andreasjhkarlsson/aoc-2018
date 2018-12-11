module Day10

open Common
open System.Drawing
open System.Drawing.Imaging

type Rect = { Left: int; Right: int; Top: int; Bottom: int}
    with
        member x.Width = x.Right - x.Left + 1
        member x.Height = x.Bottom - x.Top + 1
        member x.Area = (uint64 x.Width) * (uint64 x.Height)

[<Day(10, "The Stars Align")>]
let solve input =

    let points =
        input
        |> parseLinesAs (
            function
            | Regex @"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>" [x;y;dx;dy] -> ((int x, int y), (int dx, int dy))
            | _ -> failwith "Invalid input"
        )
        |> List.ofSeq

    let advance time = List.map (fun ((x,y), (dx,dy)) -> x + dx * time, y + dy * time)

    let bounds points =    
        {
            Left = points |> List.map fst |> List.min
            Right = points |> List.map fst |> List.max
            Top = points |> List.map snd |> List.min
            Bottom = points |> List.map snd |> List.max
        }

    let write filename scale points =

        let bounds = bounds points

        use bitmap = new Bitmap(bounds.Width * scale, bounds.Height * scale, PixelFormat.Format32bppRgb)

        use graphics = Graphics.FromImage(bitmap)
        graphics.Clear(Color.Black)

        points |> List.iter (fun (x, y) ->
            [0..(scale-1)] |*| [0..(scale-1)]
            |> Seq.iter (fun (dx,dy) ->
                bitmap.SetPixel ((x - bounds.Left)*scale + dx, (y - bounds.Top)*scale + dy, Color.LightSalmon)
            )
        )

        bitmap.Save filename

    let weight = List.map (fst >> double) >> List.average

    let time, converged =
        Seq.initInfinite id
        |> Seq.map (fun time -> points |> advance time)
        |> Seq.indexed
        |> Seq.windowed 3
        |> Seq.find (fun [|(_,a);(_,b);(_,c)|] ->
            
            let (ba, bb, bc) = bounds a, bounds b, bounds c

            ba.Area > bb.Area && bb.Area < bc.Area
        )
        |> (fun [|_;r;_|] -> r)

    let image = "part1.png"

    converged |> write image 5
    
    {
        Part1 = image
        Part2 = time
    }
