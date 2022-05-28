module ListOps

let rec foldl folder state list =
    match list with
    | [] -> state
    | x :: xs -> foldl folder (folder state x) xs

let rec foldr folder state list =
    match list with
    | [] -> state
    | x :: xs -> folder x (foldr folder state xs)

let length list =
    let rec length' lst acc =
        match lst with
        | [] -> acc
        | _ :: xs -> length' xs (acc + 1)
    length' list 0

let reverse list =
    let rec reverse' lst acc =
        match lst with
        | [] -> acc
        | x :: xs -> reverse' xs (x :: acc)
    reverse' list []

let map f list =
    let rec map' lst acc =
        match lst with
        | [] -> acc
        | x :: xs -> map' xs (acc @ [f x])
    map' list []

let filter f list =
    let rec map' lst acc =
        match lst with
        | [] -> acc
        | x :: xs -> map' xs (if f x then acc @ [x] else acc)
    map' list []

let append list1 list2 =
    list1 @ list2

let concat list =
    let rec concat' lst acc =
        match lst with
        | [] -> acc
        | x :: xs -> concat' xs (acc @ x)
    concat' list []