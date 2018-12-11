module Day11

open Common

[<Day(11, "Chronal Charge")>]
let solve input =

    let serialNo = int (String.trim input)

    let power (x,y) =
        let rackId = x + 10
        rackId
        |> (*) y
        |> (+) serialNo
        |> (*) rackId
        |> (fun pl -> (pl / 100) % 10 - 5)
        
    let divide (width, height) squareSize =
        [1..width]
        |> Seq.windowed squareSize
        |> Seq.map (fun col ->
            [1..height]
            |> List.windowed squareSize
            |> List.map ((|*|) col)
        )
        |> Seq.concat

    let mostPower = 
        Seq.map (fun sg -> sg, sg |> Seq.sumBy power)
        >> Seq.maxBy snd
        >> (fun (s, power) -> let x,y = Seq.head s in x, y, power)    

    {
        Part1 = 
            divide (300, 300) 3
            |> mostPower
            |> fun (x,y,_) -> sprintf "%d,%d" x y

        Part2 =
            [1..300]
            |> Seq.map (fun ss -> divide (300, 300) ss |> mostPower, ss)
            |> Seq.takeWhile (fun ((_,_,p), _) -> p > 0)
            |> Seq.maxBy (fun ((_,_,p), _) -> p)
            |> (fun ((x,y,_), ss) -> sprintf "%d,%d,%d" x y ss)
    }

