module PascalsTriangle

let rec private triangle (n : int) (row : int list) (acc : int list list) : int list list =
    if n = 0 then
        acc
    else
        let nextRow = 0 :: ((List.zip (List.take (row.Length - 1) row) (List.tail row)) |> List.map (fun (a, b) -> a + b)) @ [0]
        let rows = acc @ [row]
        triangle (n - 1) nextRow rows

let rows numberOfRows : int list list =
    triangle numberOfRows [0; 1; 0] [] |> List.map (fun r -> r |> List.take (r.Length - 1) |> List.tail)