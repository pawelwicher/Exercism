module Bowling

    type Frame =
        | Strike
        | Spare of int
        | Open of int 
        | Roll of int
        | BonusRoll of int
        | Error 
    type Game = Frame list

    let newGame():Game = []

    let isSpare = function
        | Spare _ -> true
        | _ -> false

    let isStrike = function
        | Strike -> true
        | _ -> false 

    let isBonus = function
        | BonusRoll _ -> true
        | _ -> false

    let missingFrames game = 
        List.sumBy (function
            | Strike | Spare _ | Open _ -> 1
            | _ -> 0) game < 10

    let roll pins game = 
        let throw =
            match List.tryHead game with 
            | _ when pins < 0 || pins > 10 -> Error 
            | Some (Roll pins') when pins + pins' > 10 -> Error 
            | Some (Roll pins') when pins + pins' = 10 -> Spare pins
            | Some (Roll _) -> Open pins
            | Some (BonusRoll pins') when pins' < 10 && pins' + pins > 10 -> Error
            | Some (BonusRoll _) -> BonusRoll pins
            | _ when missingFrames game && pins = 10 -> Strike
            | _ when missingFrames game -> Roll pins
            | Some (Strike) | Some (Spare _) -> BonusRoll pins
            | _ -> Error

        throw :: game
        
    let scoreRoll roll =
        match roll with
        | Strike -> 10
        | Spare p -> p
        | Open p -> p
        | Roll p -> p
        | BonusRoll p -> p
        | Error -> 0

    let missingBonusRolls game =
        let bonusRolls = 
            List.rev game
            |> List.takeWhile isBonus

        let last =
            List.rev game
            |> List.skipWhile isBonus
            |> List.head
        
        match last with
        | Spare _ when List.length bonusRolls <> 1 -> true
        | Strike when List.length bonusRolls <> 2 -> true
        | _ -> false

    let bonus rolls filter game =
        List.windowed rolls game
        |> List.filter (List.head >> filter)
        |> List.collect List.tail
        |> List.sumBy scoreRoll
        
    let score game = 
        let game = List.rev game

        let score = List.filter (isBonus >> not) game 
                    |> List.sumBy scoreRoll 
        let bonusSpare = bonus 2 isSpare game
        let bonusStrike = bonus 3 isStrike game
        
        if List.exists ((=) Error) game 
            || missingFrames game
            || missingBonusRolls game
        then None
        else Some <| score + bonusSpare + bonusStrike