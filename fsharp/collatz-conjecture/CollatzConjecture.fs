module CollatzConjecture

let steps (n: int): int option =
    let rec count (num: int) (acc: int) =
        if num = 1 then
            acc
        elif num % 2 = 0 then
            count (num / 2) (acc + 1)
        else
            count (3 * num + 1) (acc + 1)

    if n <= 0 then
        None
    else
        (count n 0) |> Some