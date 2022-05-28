module ArmstrongNumbers

let isArmstrongNumber (n: int): bool =
    let rec getDigits (num: int) (acc: int list) : int list =
        if num = 0 then acc else getDigits (num / 10) ((num % 10) :: acc)
    let digits = getDigits n []
    let len = digits |> Seq.length
    let sum = digits |> Seq.sumBy (fun x -> pown x len)
    sum = n