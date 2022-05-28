module Accumulate

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list =
    let rec accumulateRec (input: 'a list) (acc: 'b list): 'b list =
        match input with
        | head :: tail -> accumulateRec tail (func head :: acc)
        | [] -> acc |> List.rev

    accumulateRec input []