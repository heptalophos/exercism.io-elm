module Hamming exposing (distance)

import Result
import String exposing (length, toList)
import List exposing (map2, foldl)


distance : String -> String -> Result String Int
distance left right =
    if length left /= length right then
        Err "left and right strands must be of equal length"
    else
        left |> toList
        |> (toList >> map2 (\x y -> if x == y then 0 else 1)) right
        |> foldl (\x sum -> sum + x) 0
        |> Ok   