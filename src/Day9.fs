module Day9

open Common
open System

module Circular =

    type Direction = CW | CCW
    
    type Item = { Value: int; Previous: int; Next: int}

    type List = { Current: int; Map: Map<int, Item> }

    let empty = { Current = -1; Map = Map.empty}

    let add item after ({ Current = current; Map = map} as list) =
        if current = -1 then
            { Current = item; Map = [item => {Value = item; Previous = item; Next = item}] |> Map.ofList}
        else
            let after = map |> Map.find after
            let map = map |> Map.add after.Value { after with Next = item}
            let afterAfter = map |> Map.find after.Next
            let map = map |> Map.add afterAfter.Value { afterAfter with Previous = item }

            {
                list with
                    Map =
                        map
                        |> Map.add item { Value = item; Previous = after.Value; Next = afterAfter.Value }
            }

    let remove item ({ Current = current; Map = map} as list) = 
        let dis = map |> Map.find item
        let before = map |> Map.find dis.Previous
        let after = map |> Map.find dis.Next
        {
            list with
                Map =
                    map
                    |> Map.remove item
                    |> Map.add before.Value {before with Next = dis.Next}
                    |> Map.add after.Value {after with Previous = dis.Previous}
        }
    
    let setCurrent item list = { list with Current = item }
    
    let walk direction steps (list: List) =
        let rec walk steps (current: Item) =
            if steps = 0 then current.Value
            else
                match direction with
                | CW -> walk (steps-1) (list.Map |> Map.find current.Next)
                | CCW -> walk (steps-1) (list.Map |> Map.find current.Previous)
        walk steps (list.Map |> Map.find list.Current)



type Game = { Marbles: Circular.List; Players: Map<int, uint64>}
    with static member HighScore {Players = players} = players |> Map.toList |> List.map snd |> List.max

[<Day(9, "")>]
let solve input = 
    
    let playerCount, lastMarble =
        match input with
        | Regex @"(\d+) players; last marble is worth (\d+) points" [players; lastMarble] ->
            int players, int lastMarble
        | _ -> failwith "Invalid input"

    let play playerCount lastMarble =
        let game = {Marbles = Circular.empty; Players = Map.empty}
        
        {0..lastMarble}
        |> Seq.fold (fun game marble ->
            if marble % 10000 = 0 then printfn "%A" marble

            if marble = 0 then {game with Marbles = game.Marbles |> Circular.add 0 -1}
            elif marble % 23 = 0 then
                
                let currentScore = game.Players |> Map.tryFind (marble % playerCount) |> Option.defaultValue 0UL

                let nextCurrent = game.Marbles |> Circular.walk Circular.CCW 6
                let toRemove = game.Marbles |> Circular.walk Circular.CCW 7
                {
                    game with
                        Players = game.Players |> Map.add (marble % playerCount) (currentScore + (uint64 marble) + (uint64 toRemove))
                        Marbles = game.Marbles |> Circular.remove toRemove |> Circular.setCurrent nextCurrent
                }
                
            else
                {
                    game with
                        Marbles =
                            game.Marbles
                            |> Circular.add marble (game.Marbles |> Circular.walk Circular.CW 1)
                            |> Circular.setCurrent marble
                }
            
        ) game

    {
        Part1 = play playerCount lastMarble |> Game.HighScore
        Part2 = play playerCount (lastMarble * 100) |> Game.HighScore
    }