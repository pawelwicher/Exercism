module Isogram

let isIsogram (input: string) : bool =
    let s = input.ToLower() |> Seq.filter System.Char.IsLetter
    s |> Seq.length = (s |> Seq.distinct |> Seq.length)