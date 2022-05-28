module Triangle

let trinagleCond (triangle : float list) : bool =
    match triangle with
    | [a; b; c] -> a > 0. && b > 0. && c > 0. && a + b > c && a + c > b && b + c > a
    | _ -> false

let distinctSidesLen (triangle : float list) : int =
    triangle |> List.distinct |> List.length

let equilateral (triangle : float list) : bool =
    let isTriangle = triangle |> trinagleCond
    let sidesLen = triangle |> distinctSidesLen
    isTriangle && sidesLen = 1

let isosceles (triangle : float list) : bool =
    let isTriangle = triangle |> trinagleCond
    let sidesLen = triangle |> distinctSidesLen
    isTriangle && (sidesLen = 1 || sidesLen = 2)

let scalene (triangle : float list) : bool =
    let isTriangle = triangle |> trinagleCond
    let sidesLen = triangle |> distinctSidesLen
    isTriangle && sidesLen = 3