module BookStore

let msrp = 8.0m 

let computeDiscount count = 
    match count with 
    | 2 -> 0.95m
    | 3 -> 0.9m
    | 4 -> 0.8m
    | 5 -> 0.75m
    | _ -> 1.0m

let total books = 
    let rec bundle cart order =
        let size = order |> List.filter (fun number -> number <= List.length cart) |> List.head
        let subtotal = computeDiscount (size) * (decimal size) * msrp
        let shrink = (cart |> List.take(size) |>  List.map (fun num -> num - 1) |> List.filter (fun c -> c > 0)) @ (cart |> List.skip(size))
        subtotal + if List.isEmpty shrink then 0.0m else  (bundle shrink order)

    let distinct = books |> List.groupBy id |> List.map (fun (_, instances) -> List.length instances) |> List.sortDescending

    seq {bundle distinct [5;4;3;2;1;0]; 
         bundle distinct [4;3;5;2;1;0];
         bundle distinct [4;5;3;2;1;0]; }
    |> Seq.min