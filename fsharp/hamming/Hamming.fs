module Hamming

let distance (s1: string) (s2: string): int option =
    if s1.Length = s2.Length then
        (s1, s2) ||> Seq.zip
                  |> Seq.filter (fun (c1, c2) -> c1 <> c2) 
                  |> Seq.length 
                  |> Some
    else
        None