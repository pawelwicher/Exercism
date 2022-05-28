module Bob

let (|Empty|_|) (input : string) =
    if System.String.IsNullOrWhiteSpace(input) then Some () else None

let (|Capitalized|_|) (input : string) =
    if input = input.ToUpper() && input <> input.ToLower() then Some () else None

let (|Question|_|) (input : string) =
    if input.TrimEnd().EndsWith('?') then Some () else None

let response = function
    | Empty -> "Fine. Be that way!"
    | Capitalized & Question _ -> "Calm down, I know what I'm doing!"
    | Capitalized -> "Whoa, chill out!"
    | Question -> "Sure."
    | _ -> "Whatever."