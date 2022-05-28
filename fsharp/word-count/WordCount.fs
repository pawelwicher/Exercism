module WordCount

let countWords (phrase : string) =
    let words =
        System.Text.RegularExpressions.Regex.Matches(phrase.ToLower(), "\w+('\w+)?") 
        |> Seq.map (fun x -> x.Value)
        |> Seq.toList

    let addWord (word : string) dict =
        if Map.containsKey word dict then
            Map.add word ((Map.find word dict) + 1) dict
        else
            Map.add word 1 dict 

    let rec count (words : string list) dict =
        match words with
        | [] -> dict
        | word :: restWords -> count restWords (addWord word dict)

    count words Map.empty