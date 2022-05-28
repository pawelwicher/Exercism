module ValentinesDay

type Approval = Yes | No | Maybe

type Cuisine = Korean | Turkish

type Genre = Crime | Horror | Romance | Thriller

type Activity =
    | BoardGame
    | Chill
    | Movie of Genre
    | Restaurant of Cuisine
    | Walk of int

let rateActivity (activity: Activity): Approval =
    match activity with
    | BoardGame -> No
    | Chill -> No
    | Movie Crime -> No
    | Movie Horror -> No
    | Movie Romance -> Yes
    | Movie Thriller -> No
    | Restaurant Korean -> Yes
    | Restaurant Turkish -> Maybe
    | Walk 1 -> Yes
    | Walk 2 -> Yes
    | Walk 3 -> Maybe
    | Walk 4 -> Maybe
    | Walk n when n >= 5 -> No
    | _ -> No
