module BinarySearch

let find (xs : int array) (item : int) : int option =
    let rec find' (xs : int array) (item : int) (l : int) (r : int) : int option =
        if r >= l then
            let mid = l + (r - l) / 2
            if xs.[mid] = item then
                Some mid
            elif xs.[mid] > item then
                find' xs item l (mid - 1)
            else
                find' xs item (mid + 1) r
        else
            None

    find' xs item 0 (xs.Length - 1)
