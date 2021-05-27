module Luhn exposing (valid)


valid : String -> Bool
valid input =

    let 
        compact = 
            String.filter (not << (==) ' ')

        isDirty = 
            String.any (Char.isDigit >> not) 

        sanitized x = 
            if x |> compact |> isDirty then "" else x 

        luhn x = 
            case x of
                [] -> 0
                [z] -> z
                (x0::x1::xs) -> 
                    x0 + 2 * x1 - 
                    (if x1 >= 5 then 9 else 0) + 
                    (luhn xs)

        checksum = 
            String.toList 
            >> List.filterMap (String.fromChar >> String.toInt)
            >> List.reverse
            >> luhn 
            >> modBy 10

    in
        case input |> compact |> sanitized of
            ""  -> False
            "0" -> False
            x   -> checksum x == 0            
