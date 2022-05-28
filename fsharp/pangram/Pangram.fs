module Pangram

let isPangram (input: string): bool =
    let s = input.ToLower()
    { 'a' .. 'z' } |> Seq.map (fun c -> Seq.contains c s) |> Seq.forall id