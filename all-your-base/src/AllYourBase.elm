module AllYourBase exposing (rebase)


rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    let 
        errors : Bool
        errors = 
            List.all (\x -> x == 0) digits
            || List.any (\x -> x < 0) digits
            || List.any (\x -> x >= inBase) digits
            || List.isEmpty digits
            || List.any (\x -> x < 2) [inBase, outBase]

        representationIn : Int -> Int -> List Int
        representationIn base num =
            let 
                digitsIn b n =
                    if n > 0 then
                        modBy b n :: digitsIn b (n // b)
                    else
                        []
            in 
                digitsIn base num |> List.reverse 
    in 
        if errors then
            Nothing
        else
            Just <| representationIn outBase 
                 <| List.foldl (\d acc -> acc * inBase + d) 
                               0 
                               digits
            