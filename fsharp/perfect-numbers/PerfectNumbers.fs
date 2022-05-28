module PerfectNumbers

type Classification = Perfect | Abundant | Deficient

let getClassification (n : int) : Classification =
    let factorsSum = [1 .. n / 2] |> List.filter (fun x -> n % x = 0) |> List.sum
    if factorsSum = n then
        Perfect
    elif factorsSum > n then
        Abundant
    else
        Deficient

let classify (n : int) : Classification option =
    if n < 1 then
        None
    else
        getClassification n |> Some
