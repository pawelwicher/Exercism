module LargestSeriesProduct

let largestProduct (input: string) (span: int) : int option =
    let digitsOnly = input.Length = (input |> Seq.filter System.Char.IsDigit |> Seq.length)
    if span >= 0 && input.Length >= span && digitsOnly then
        let charToInt (c: char) : int =
            int c - int '0'
        let getSpanProduct (pos: int) : int =
           input |> Seq.skip pos |> Seq.take span |> Seq.map charToInt |> Seq.fold (*) 1
        { 0 .. input.Length - span } |> Seq.map getSpanProduct |> Seq.max |> Some
    else
        None