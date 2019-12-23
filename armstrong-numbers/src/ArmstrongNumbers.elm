module ArmstrongNumbers exposing (isArmstrongNumber)

flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a

isArmstrongNumber : Int -> Bool
isArmstrongNumber nb =
    let 
        digits = 
            nb 
            |> String.fromInt 
            |> String.toList 
            |> List.map ( String.fromChar 
                          >> String.toInt
                          >> Maybe.withDefault 0 ) 
        digitsCount =
            digits |> List.length
    in
        digits 
        |> List.foldl (flip (^) digitsCount >> (+)) 0 
        |> (==) nb       
