module ArmstrongNumbers exposing (isArmstrongNumber)

isArmstrongNumber : Int -> Bool
isArmstrongNumber nb =
    let 
        digits = 
            nb |> String.fromInt 
               |> String.toList 
               |> List.map ( 
                    Maybe.withDefault 0 << String.toInt << String.fromChar 
                ) 
        digitsCount =
            digits |> List.length
        flip fcn x y = fcn y x
    in
        digits |> List.foldl ((+) << flip (^) digitsCount) 0 
               |> (==) nb       
