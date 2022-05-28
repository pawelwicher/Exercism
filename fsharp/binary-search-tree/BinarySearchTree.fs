module BinarySearchTree

type Node =
  | Empty
  | Node of item: int * left: Node * right: Node

let left (node: Node): Node option =
    match node with
    | Node (_, left, _) when left <> Empty -> Some left
    | _ -> None

let right (node: Node): Node option =
    match node with
    | Node (_, _, right) when right <> Empty -> Some right
    | _ -> None

let data (node: Node): int =
    match node with
    | Node (item, _, _) -> item
    | Empty -> failwith "empty node"

let create (items: int list): Node =
    let rec insert (root: Node) (value: int): Node =
        match root with
        | Empty -> Node (value, Empty, Empty)
        | Node (item, left, right) when value <= item -> Node (item, insert left value, right)
        | Node (item, left, right) -> Node (item, left, insert right value)
    items |> List.fold insert Empty

let rec sortedData (node: Node): int list =
    match node with
    | Empty -> []
    | Node (item, left, right) -> sortedData left @ [item] @ sortedData right