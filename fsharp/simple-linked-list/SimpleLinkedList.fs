module SimpleLinkedList

type Node =
    | Nil
    | Node of int * Node

let nil = Nil

let create (x : int) (node : Node) : Node = Node(x, node)

let isNil (node : Node) : bool = node = nil

let next (node : Node) : Node =
    match node with
    | Nil -> Nil
    | Node(_, n) -> n

let datum (node : Node) : int =
    match node with
    | Nil -> failwith "Nil"
    | Node(x, _) -> x

let rec toList (node : Node) : int list =
    match node with
    | Nil -> []
    | Node(x, _) -> x :: toList (next node)

let rec fromList (xs : int list) : Node =
    match xs with
    | [] -> Nil
    | x :: xss -> Node(x, fromList xss)

let reverse (node : Node) : Node =
    node |> toList |> List.rev |> fromList