module Luhn exposing (valid)


valid : String -> Bool
valid input =
    let 
        luhn x = case x of
            [] -> 0
            [z] -> z
            (x0::x1::xs) -> 
                 x0 + 2 * x1 - (if x1 >= 5 then 9 else 0) + (luhn xs)
        checksum = 
            String.toList 
            >> List.filterMap (String.fromChar >> String.toInt)
            >> List.reverse
            >> luhn 
            >> (modBy 10)
        sanitized x = if x |> String.any (Char.isDigit >> not) >> not 
                      then "" 
                      else x 
    in
        case input |> String.filter (not << (==) ' ') of
            "" -> False
            -- [_] -> False
            x -> checksum x == 0            
