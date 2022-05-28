module TwelveDays

let recite start stop =
    let dayNumbers = ["first"; "second"; "third"; "fourth"; "fifth"; "sixth"; "seventh"; "eighth"; "ninth"; "tenth"; "eleventh"; "twelfth"]
    let counts = ["a"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten"; "eleven"; "twelve"]
    let presents = [
      "Partridge in a Pear Tree"
      "Turtle Doves"
      "French Hens"
      "Calling Birds"
      "Gold Rings"
      "Geese-a-Laying"
      "Swans-a-Swimming"
      "Maids-a-Milking"
      "Ladies Dancing"
      "Lords-a-Leaping"
      "Pipers Piping"
      "Drummers Drumming"
    ]
    let cite dayNumber presents = sprintf "On the %s day of Christmas my true love gave to me: %s." dayNumber presents
    let presentsCite n =
      [0 .. n - 1] 
      |> List.map (fun x -> (if x = 0 && n > 1 then "and " else "") + counts.[x] + " " + presents.[x])
      |> List.rev
      |> String.concat ", "
    [start .. stop]
    |> List.map (fun n -> cite dayNumbers.[n - 1] (presentsCite n))

