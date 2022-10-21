module Leap exposing ( isLeapYear )


isLeapYear : Int -> Bool
isLeapYear year = 
    let 
        r x = modBy x year
    in
        case (r 400, r 100 ,r 4) of
            (0, 0, 0) -> True
            (_, 0, 0) -> False
            (_, _, 0) -> True
            (_, _, _) -> False