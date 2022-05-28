module CustomSet

type MySet =
    | Empty
    | Set of int list

let empty =
    Empty

let singleton value =
    Set [value]

let isEmpty set =
    set = Empty

let size set =
    match set with
    | Set xs -> xs.Length
    | Empty -> 0

let fromList xs =
    match xs with
    | [] -> Empty
    | _ -> Set (xs |> List.sort)

let toList set =
    match set with
    | Set xs -> xs
    | Empty -> []

let contains value set =
    match set with
    | Set xs -> xs |> List.contains value
    | Empty -> false

let insert value set =
    match set with
    | Set xs -> if xs |> List.contains value then Set xs else value :: xs |> fromList
    | Empty -> singleton value

let union left right =
    match left, right with
    | Empty, Empty -> Empty
    | Empty, Set xs -> Set xs
    | Set xs, Empty -> Set xs
    | Set xs1, Set xs2 -> (xs1 @ xs2) |> List.distinct |> fromList

let intersection left right =
    match left, right with
    | Empty, Empty -> Empty
    | Empty, Set _ -> Empty
    | Set _, Empty -> Empty
    | Set xs1, Set xs2 -> xs1 |> List.filter (fun x -> xs2 |> List.contains x) |> fromList

let difference left right =
    match left, right with
    | Empty, Empty -> Empty
    | Empty, Set _ -> Empty
    | Set xs, Empty -> Set xs
    | Set xs1, Set xs2 -> (xs1 |> List.filter (fun x -> xs2 |> List.contains x |> not)) |> fromList

let isSubsetOf left right =
    intersection right left = left

let isDisjointFrom left right =
    intersection left right = Empty

let isEqualTo left right =
    left = right