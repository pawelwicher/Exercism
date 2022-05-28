module QueenAttack

let create (position: int * int) =
    match position with
    | (row, column) when row = column && row >= 0 && row < 8 && column >= 0 && column < 8 -> true
    | _ -> false 

let canAttack (queen1: int * int) (queen2: int * int) =
    match (queen1, queen2) with
    | ((q1row, q1column), (q2row, q2column)) when 
       q1row = q2row ||
       q1column = q2column ||
       abs (q1row - q2row) = abs (q1column - q2column) -> true
    | _ -> false