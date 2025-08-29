module EliudsEggs exposing (eggCount)

import Bitwise

eggCount : Int -> Int
eggCount = 
    positionalEggs >> List.sum

positionalEggs : Int -> List Int
positionalEggs n = 
    case n == 0 of
        True -> 
            [] 
        _ -> 
            Bitwise.and 1 n :: 
            positionalEggs (Bitwise.shiftRightBy 1 n)
