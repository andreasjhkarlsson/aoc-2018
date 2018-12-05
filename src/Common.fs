module Common

open System.Reflection
open System
open System.IO
open System.Text
open System.Text.RegularExpressions

type Solution<'a,'b> =
    {
        Part1: 'a
        Part2: 'b
    }
    with static member Pair (p1,p2) = { Part1 = p1; Part2 = p2 }

type DayAttribute (day, title) =
    inherit System.Attribute ()

    member x.Day: int = day

    member x.Title: string = title

    with
        static member All =
            Assembly.GetExecutingAssembly().GetTypes() 
            |> Array.collect (fun typ -> typ.GetMethods())
            |> Array.choose (fun mi -> 
                let attr = mi.GetCustomAttribute(typeof<DayAttribute>)
                if attr <> null then Some (attr :?> DayAttribute, mi)
                else None)
            |> Array.sortBy (fun (d, _) -> d.Day)
            |> Array.toList

type Console with
    static member ReadAll () =
        use stdin = Console.OpenStandardInput ()
        use reader = new StreamReader(stdin, Encoding.UTF8)
        reader.ReadToEnd () 

module Seq =
    let repeat items = seq { while true do yield! items }

    let count items = items |> Seq.countBy (fun e -> e)
 
module String =
    let trim (str: string) = str.Trim ()


let parseLines (str: string) =
    str.Split([|'\r'; '\n'|])
   |> Array.choose (fun row ->
        match row.Trim() with
        | "" -> None
        | "\n" -> None
        | row -> Some row)
    |> Seq.ofArray

let parseLinesAs fn = parseLines >> Seq.map fn

let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

let (=>) a b = a, b

// Cartesian product operator
let (|*|) l1 l2 =
    l1
    |> Seq.map (fun e1 -> l2 |> List.map ((=>) e1))
    |> Seq.concat