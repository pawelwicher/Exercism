module Grains

let square (n: int): Result<uint64,string> =
    if n > 0 && n < 65 then Ok (pown 2UL (n - 1)) else Error "square must be between 1 and 64"

let total: Result<uint64,string> =
    Ok ({ 0 .. 63 } |> Seq.sumBy (fun x -> pown 2UL x))