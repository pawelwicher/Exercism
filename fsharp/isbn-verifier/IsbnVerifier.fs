module IsbnVerifier

let isValid (isbn : string) =
    isbn.ToCharArray()
    |> Seq.filter (fun c -> System.Char.IsDigit c || c = 'X')
    |> Seq.mapi (fun i c -> (i, c))
    |> Seq.filter (fun (i, c) -> c <> 'X' || (c = 'X' && i = 9))
    |> Seq.map (fun (_, c) -> c)
    |> fun x -> if Seq.length x = 10 then x else Seq.empty
    |> Seq.map (fun c -> if c = 'X' then 10 else (System.Char.GetNumericValue >> int) c)
    |> Seq.zip [10 .. -1 .. 1]
    |> Seq.map (fun (a, b) -> a * b)
    |> Seq.sum
    |> fun x -> x > 0 && x % 11 = 0