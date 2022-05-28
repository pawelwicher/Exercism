module NucleotideCount

let nucleotideCounts (strand: string): Option<Map<char, int>> =
    let nucs = strand.ToCharArray() |> Seq.ofArray
    let allNucsCount = nucs |> Seq.length
    let countNucs nuc = nucs |> Seq.filter ((=) nuc) |> Seq.length
    let aNucsCount = countNucs 'A'
    let cNucsCount = countNucs 'C'
    let gNucsCount = countNucs 'G'
    let tNucsCount = countNucs 'T'  

    if allNucsCount = aNucsCount + cNucsCount + gNucsCount + tNucsCount then
        [('A', aNucsCount); ('C', cNucsCount); ('G', gNucsCount); ('T', tNucsCount)] |> Map.ofList |> Some
    else
        None
