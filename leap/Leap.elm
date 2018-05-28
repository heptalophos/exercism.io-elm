module Leap exposing (isLeapYear)


isLeapYear : Int -> Bool
isLeapYear year =
    case ( year % 4 == 0, year % 100 == 0, year % 400 == 0 ) of
        ( True, True, True )   -> True
        ( True, True, False )  -> False
        ( True, False, False ) -> True
        ( True, False, True )  -> True
        ( False, _, _ )        -> False
