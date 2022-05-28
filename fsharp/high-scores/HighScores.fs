module HighScores

let scores (values: int list): int list =
    values

let latest (values: int list): int =
    values |> List.last 

let personalBest (values: int list): int =
    values |> List.max

let personalTopThree (values: int list): int list =
    let xs = values |> List.sort |> List.rev
    if xs |> List.length > 3 then xs |> List.take 3 else xs