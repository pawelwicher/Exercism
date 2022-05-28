module WordSearch

let toMatrix (grid:string list) = 
    grid
    |> List.map (fun x -> x.ToCharArray() |> List.ofArray)
    |> array2D

let toString (ary:char seq) = 
    ary |> Seq.fold (fun acc x -> acc + x.ToString()) ""

let (right,left,col,down,up,row) = (1,-1,0,1,-1,0)

let slice (x',y') right down  i (grid:string list) = 
    let (x,y) = (x'-1,y'-1)
    let ary = grid |> toMatrix
    let incrementedRow = y + (i - 1) * down
    let incrementedCol = x + (i - 1) * right
    if incrementedRow > ary.GetUpperBound 0
        || incrementedRow < 0
        || incrementedCol > ary.GetUpperBound 1
        || incrementedCol < 0 then
        "", None
    else
        (seq{
            for i' in [0..(i - 1)] do
                 yield ary.[y + (i' * down), x + (i' * right)]
        } |> toString), Some ((x',y'), (incrementedCol+1,incrementedRow+1))

let indexes ch grid = 
    let ary = grid |> toMatrix
    let rec index' acc r c =
          if   c >= ary.GetLength 1 then acc
          elif r >= ary.GetLength 0 then index' acc 0 (c+1)
          elif ary.[r,c] = ch   then index' (acc@[(c+1,r+1)]) (r+1) c 
          else index' acc (r+1) c
    index' [] 0 0

let distrib x xs = 
    let ys = xs |> List.filter (fun x' -> x' <> x)
    ys
    |> List.map (fun x' -> (x,x'))

let rec permute xs = 
    xs
    |> List.map (fun x -> (xs |> distrib x)@[x,x])
    |> List.concat

let find (str:string) grid = 
    let fst = str.ToCharArray().[0]
    let indexes = grid |> indexes fst
    let finded = 
        [0;1;-1] 
        |> permute
        |> List.map (fun (horiz,vert) -> 
                indexes
                |> List.map (fun x -> grid |> slice x horiz vert str.Length)
                |> List.filter (fun (s,i) -> s = str)
            )
        |> List.concat
    if finded.Length > 0 then
        finded
    else
        [(str, Option<((int * int) * (int * int))>.None)] 

let search grid (wordsToSearchFor:string list) =
    wordsToSearchFor
    |> List.map (fun x -> find x grid)
    |> List.concat
    |> Map.ofList