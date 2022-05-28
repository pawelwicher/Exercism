module NthPrime

let isPrime n =
    match n with
    | 1 -> false
    | 2 -> true
    | _ when n % 2 = 0 || [3 .. 2 .. int (sqrt (float n)) + 1] |> Seq.exists (fun x -> n % x = 0) -> false
    | _ -> true

let prime' n =
    Seq.initInfinite ((+) 1) |> Seq.filter isPrime |> Seq.item (n - 1)

let prime n =
    if n > 0 then
        prime' n |> Some
    else
        None