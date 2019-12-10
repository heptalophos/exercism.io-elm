module Leap exposing (isLeapYear)


isLeapYear : Int -> Bool
isLeapYear year =
    case (modBy 400 year == 0, 
          modBy 100 year == 0, 
          modBy 4 year   == 0) 
    of
        (True,True, True) -> True
        (False,True,True) -> False
        (_,False,True)    -> True
        (_, _, _)         -> False