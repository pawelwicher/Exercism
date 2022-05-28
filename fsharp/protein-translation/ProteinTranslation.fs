module ProteinTranslation

let proteins (rna : string) : string list =
    let codonToProtein = function
        | "AUG" -> "Methionine"
        | "UUU" | "UUC" -> "Phenylalanine"
        | "UUA" | "UUG" -> "Leucine"
        | "UCU" | "UCC" | "UCA" | "UCG" -> "Serine"
        | "UAU" | "UAC" -> "Tyrosine"
        | "UGU" | "UGC" -> "Cysteine"
        | "UGG" -> "Tryptophan"
        | _ -> failwith "error"

    rna 
    |> Array.ofSeq
    |> Seq.chunkBySize 3
    |> Seq.map System.String
    |> Seq.takeWhile (fun x -> List.contains x ["UAA"; "UAG"; "UGA"] |> not)
    |> Seq.map codonToProtein
    |> Seq.toList