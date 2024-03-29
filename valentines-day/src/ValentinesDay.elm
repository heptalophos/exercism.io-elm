module ValentinesDay exposing (..)


type Approval = Yes | No | Maybe


type Genre = Crime | Horror | Romance | Thriller


type Cuisine = Korean | Turkish


type Activity = BoardGame 
                | Chill 
                | Movie Genre 
                | Restaurant Cuisine


rateActivity : Activity -> Approval
rateActivity activity =
    case activity of 
        Movie Romance -> 
            Yes
        Movie _ ->
            No
        Restaurant Korean -> 
            Yes
        Restaurant Turkish -> 
            Maybe
        _ -> 
            No