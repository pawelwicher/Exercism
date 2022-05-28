module Pov

type 'a Graph = {
    value : 'a
    children : 'a Graph list 
}

let mkGraph value children = { 
    value = value
    children = children 
}

let remove child parent = 
    parent.children 
    |> List.filter (fun c -> c.value <> child.value)

let updateChildren tree children = {
    tree with children = children
}

let moveParentToChild oldParent oldChild =
    (remove oldChild oldParent |> updateChildren oldParent)
    :: oldChild.children
    |> updateChildren oldChild

let pathToItem value tree =
    let rec pool t acc =
        match t.value = value with
        | true -> t :: acc |> List.rev |> Some
        | false -> 
            t.children 
            |> List.choose (fun c -> (t :: acc) |> pool c)
            |> List.tryFind (fun _ -> true)
    pool tree []

let fromPOV (value : 'a) (tree : 'a Graph) =
    match pathToItem value tree with
    | Some path -> 
        path 
        |> List.reduce moveParentToChild 
        |> Some
    | None -> None

let tracePathBetween origin destination tree =
    fromPOV origin tree 
    |> Option.bind (pathToItem destination) 
    |> Option.map (List.map (fun t -> t.value))