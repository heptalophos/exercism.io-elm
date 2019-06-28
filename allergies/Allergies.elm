module Allergies exposing (isAllergicTo, toList)

import Dict exposing (Dict, fromList, get, keys)
import List exposing (filter, map)
import Maybe exposing (withDefault)
import Bitwise exposing (and)


isAllergicTo : String -> Int -> Bool
isAllergicTo name score =
    get name allergies
    |> withDefault 0
    |> and score >> (/=) 0


toList : Int -> List String
toList score =
    keys allergies
    |> filter (flip isAllergicTo score) 

-- auxiliary

flip : (a -> b -> c) -> b -> a -> c
flip fn a b = fn b a


allergies : Dict String Int
allergies = [ ("eggs",          1), 
              ("peanuts",       2),
              ("shellfish",     4),
              ("strawberries",  8),
              ("tomatoes",      16),
              ("chocolate",     32),
              ("pollen",        64),
              ("cats",          128) ]
            |> fromList 