module PhoneNumber

open System

let clean (input: string) : Result<uint64, string> =
    let xs = input.ToCharArray() |> Array.toList |> List.filter (fun x -> List.contains x ['('; ')'; '+'; '-'; '.'; ' '] |> not)
    let n = xs |> List.length
    match xs with
    | _ when xs |> List.exists Char.IsLetter       -> Error "letters not permitted"
    | _ when xs |> List.forall Char.IsDigit |> not -> Error "punctuations not permitted"
    | _ when n > 11                                -> Error "more than 11 digits"
    | _ when n < 10                                -> Error "incorrect number of digits"
    | _ when n = 11 && xs.[0] <> '1'               -> Error "11 digits must start with 1"
    | _ when xs.[n - 10] = '0'                     -> Error "area code cannot start with zero"
    | _ when xs.[n - 10] = '1'                     -> Error "area code cannot start with one"
    | _ when xs.[n - 7] = '0'                      -> Error "exchange code cannot start with zero"
    | _ when xs.[n - 7] = '1'                      -> Error "exchange code cannot start with one"
    | _                                            -> Ok ((if n = 11 then List.tail xs else xs) |> List.toArray |> String |> uint64)