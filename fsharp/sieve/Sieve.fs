module Sieve

let primes limit =
    if limit < 2 then
        []
    else
        let a = Array.init (limit + 1) (fun _ -> true)
        let last = System.Math.Sqrt(float limit) |> int
        for i in 2 .. last + 1 do
            if a.[i] then
                for j in i * 2 .. i .. limit do
                    a.[j] <- false
        seq { for i in 2 .. limit do if a.[i] then yield i } |> Seq.toList