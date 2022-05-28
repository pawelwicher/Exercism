module Acronym

let abbreviate (phrase : string) =
    phrase.Split [|' '; '_'; '-'|]
    |> Seq.filter (fun x -> x.Length > 0)
    |> Seq.map (fun x -> x[0].ToString().ToUpper())
    |> String.concat ""