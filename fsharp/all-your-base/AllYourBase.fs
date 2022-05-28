module AllYourBase

let rebase (digits: int list) (inputBase: int) (outputBase: int) : int list option =
    if inputBase > 1 && outputBase > 1 && Seq.forall (fun x -> x >= 0 && x < inputBase) digits then

        let base10Number = 
            { Seq.length digits - 1 .. -1 .. 0 }
            |> Seq.zip digits
            |> Seq.fold (fun acc (x, n) -> acc + x * pown inputBase n ) 0

        let rec getDigits x acc =
            if x = 0 then acc else getDigits (x / outputBase) acc @ [x % outputBase]

        Some (if base10Number = 0 then [0] else getDigits base10Number [])
    else
        None