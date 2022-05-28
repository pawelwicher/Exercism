module Clock

let create hours minutes =
    let realModulo n m = (n % m + m) % m
    let minutesTotal = realModulo (hours * 60 + minutes) 1440 
    (minutesTotal / 60, minutesTotal % 60)

let add minutes (h, m) =
    create h (m + minutes)

let subtract minutes (h, m) =
    create h (m - minutes)

let display (h, m) =
    sprintf "%02i:%02i" h m
