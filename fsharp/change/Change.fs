module Change

let findFewestCoins (coins: int list) (amount: int) : int list option =
    if amount > 0 then
        let result = Array.init (amount + 1) (fun _ -> amount + 1)
        let coinsResults: int array array = Array.init (amount + 1) (fun _ -> [||])
        result.[0] <- 0
        for i in 1 .. amount do
            for coin in coins do
                if i >= coin && result.[i - coin] + 1 < result.[i] then
                    result.[i] <- result.[i - coin] + 1
                    coinsResults.[i] <- Array.concat [coinsResults.[i - coin]; [|coin|]]
        if result.[amount] = amount + 1 then
            None
        else
            coinsResults.[amount] |> List.ofArray |> List.rev |> Some
    elif amount = 0 then
        Some []
    else
        None