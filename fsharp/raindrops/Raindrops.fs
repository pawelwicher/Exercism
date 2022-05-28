module Raindrops

let convertDict =
    [
      (3, "Pling")
      (5, "Plang")
      (7, "Plong")
    ]

let convert (n: int): string =
    convertDict
    |> List.map (fun (num, text) -> if n % num = 0 then text else "")
    |> String.concat ""
    |> function  "" -> string n | text -> text