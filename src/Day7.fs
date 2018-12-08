module Day7

open Common

module Graph =
   
    type Node = Node of char * Node list
        with
            static member Name (Node (name, _))  = name
            static member Children (Node (_, children)) = children

    let sort = List.sortBy Node.Name

    let build pairs =

        let rec build name =
            Node (name, 
                pairs
                |> List.filter (fst >> ((=) name))
                |> List.map (snd >> build)
            )

        pairs
        |> List.filter (fun (name,_) -> pairs |> List.exists (snd >> ((=) name)) |> not)
        |> List.map fst
        |> List.distinct
        |> List.map build
        |> sort

    let name = Node.Name

    let rec nodes graph =
        seq {
            for node in graph do    
                yield node
                yield! nodes (Node.Children node)
        }
        |> Seq.distinctBy Node.Name

    let isDirectChild node parent =
        (Node.Children parent) |> List.exists (fun child -> (Node.Name child) = (Node.Name node))

    let rec dependsOn ((Node (a,_)) as node) (Node (b, children)) =
        if a = b then true
        else
            if children = [] then false
            else children |> List.exists (dependsOn node)

    let rec isTrueRoot node =
        nodes
        >> Seq.exists (isDirectChild node)
        >> not

    let prune node graph =
        graph
        |> List.filter (fun (Node (name,_)) -> name <> (Node.Name node))
        |> fun graph -> (Node.Children node) @ graph
        |> sort

    let rec ordered graph = 
        seq {
            if graph <> [] then
                let next = graph |> sort |> List.tryFind (fun n -> isTrueRoot n graph)
                match next with
                | Some node ->
                    yield node
                    yield! graph |> prune node |> ordered
                | None -> ()
        } |> Seq.toList
        

type Worker = Worker of Graph.Node * int
    with
        static member Done now (Worker (node, start)) = (now-start) >= ((int (Graph.name node)) - 64 + 60)

[<Day(7, "The Sum of Its Parts")>]
let solve input =

    let manual =
        input
        |> parseLinesAs (
           function
           | Regex @"Step (\w+).*step (\w+).*" [from; ``to``] -> (Seq.head from, Seq.head ``to``)
           | _ -> failwith "Invalid input" )
        |> Seq.toList

    let graph = Graph.build manual

    let rec coop time workers graph =
        let completed = workers |> List.filter (Worker.Done time)
        let workers = workers |> List.except completed

        let graph = completed |> Seq.fold (fun g (Worker (n,_)) -> g |> Graph.prune n) graph
        
        let ordered = Graph.ordered graph

        let next =
            ordered
            |> List.tryFind (fun n ->
                workers
                |> List.exists (fun (Worker (w,_)) -> Graph.dependsOn n w)
                |> not)

        match next with
        | Some next when workers.Length < 5 ->
            coop time ((Worker (next, time))::workers) graph
        | None when workers.Length = 0 ->
            time
        | _ ->
            coop (time+1) workers graph

    {
        Part1 =
            graph
            |> Graph.ordered
            |> List.map (Graph.name >> string)
            |> String.concat ""
        Part2 = coop 0 [] graph
    }