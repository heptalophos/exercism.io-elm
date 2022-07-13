module AllYourBase exposing (rebase)

import List exposing (any, all, isEmpty, foldl, reverse)


rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    let 
        errors : Bool
        errors = 
            all (\x -> x == 0) digits || 
            any (\x -> x < 0) digits || 
            any (\x -> x >= inBase) digits || 
            isEmpty digits || 
            any (\x -> x < 2) [inBase, outBase]

        representationIn : Int -> Int -> List Int
        representationIn base num =
            let 
                digitsIn b n =
                    if n > 0 then
                        modBy b n :: digitsIn b (n // b)
                    else
                        []
            in 
                digitsIn base num |> reverse 
    in 
        if errors then
            Nothing
        else
            Just <| representationIn outBase 
                 <| foldl (\d acc -> acc * inBase + d) 0 digits
            