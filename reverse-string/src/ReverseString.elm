module ReverseString exposing (reverse)

import String exposing (foldl, cons)

reverse : String -> String
reverse str=
    foldl cons "" str