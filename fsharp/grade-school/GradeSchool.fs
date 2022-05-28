module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty<int, string list>

let add student grade school =
    match Map.tryFind grade school with
    | Some students -> Map.add grade (student :: students) school
    | _             -> Map.add grade [student] school

let grade n school =
    match Map.tryFind n school with
    | Some _ -> school.[n] |> List.sort
    | _      -> []

let roster school =
    school 
    |> Map.toList 
    |> List.map fst 
    |> List.sort
    |> List.collect (fun x -> grade x school)
