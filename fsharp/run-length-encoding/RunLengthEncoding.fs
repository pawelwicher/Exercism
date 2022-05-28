module RunLengthEncoding

let encode (input : string) : string =
    let rec getCharCounts (c : char) (count : int) (chars : char list) (acc : (int * char) list) =
        match chars with
        | x :: xs -> if x = c then 
                        getCharCounts x (count + 1) xs acc
                     else
                        getCharCounts x 1 xs ((count, c) :: acc)
        | [] -> ((count, c) :: acc)
    if input = "" then
        ""
    else
        let chars = input |> Seq.toList 
        let charCounts = getCharCounts (List.head chars) 1 (List.tail chars) [] |> List.rev
        charCounts |> List.map (fun (a, b) -> (if a = 1 then "" else string a) + b.ToString()) |> String.concat ""


let decode (input : string) : string =
    let rec getCharCounts (digitChars : char list) (chars : char list) (acc : (int * char) list) =
        match chars with
        | x :: xs -> if System.Char.IsDigit x then
                        getCharCounts (digitChars @ [x]) xs acc
                     else
                        getCharCounts [] xs ((if List.isEmpty digitChars then (1, x) else (digitChars |> Seq.toArray |> System.String |> int, x)) :: acc)
        | [] -> acc
    if input = "" then
        ""
    else
        let chars = input |> Seq.toList
        let charCounts = getCharCounts [] chars [] |> List.rev
        charCounts |> List.map (fun (a, b) -> System.String(b, a)) |> String.concat ""           