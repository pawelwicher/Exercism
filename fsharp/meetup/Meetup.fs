module Meetup

open System

type Week = First | Second | Third | Fourth | Last | Teenth

let meetup (year : int) (month : int) (week : Week) (dayOfWeek : DayOfWeek) : DateTime =
    let rec findDays (date : DateTime) (days : int list) : int list =
        if date.Month <> month then
            days
        else
            let nextDate = date.AddDays(1.0)
            let newDays = if date.DayOfWeek = dayOfWeek then days @ [date.Day] else days            
            findDays nextDate newDays

    let days = findDays (DateTime(year, month, 1)) []

    let day =
        match week with
        | First  -> days.[0]
        | Second -> days.[1]
        | Third  -> days.[2]
        | Fourth -> days.[3]
        | Last   -> days |> List.last
        | Teenth -> days |> List.filter (fun x -> x > 12) |> List.head

    DateTime(year, month, day)