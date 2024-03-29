module LargestSeriesProduct exposing ( largestProduct )


import Maybe  exposing ( withDefault )
import String exposing ( any, toInt, toList, fromList, slice )
import Char   exposing ( isDigit )
import List   exposing ( foldl, indexedMap, map, range, maximum )


largestProduct : Int -> String -> Maybe Int
largestProduct length series =
    let 
        errors = 
            length < 0 || 
            length > (series |> String.length) || 
            any (not << isDigit) series
        product = 
            toList 
            >> map (\c -> fromList [c] |> toInt >> withDefault 0)      
            >> foldl (*) 1
    in 
        if errors then
            Nothing
        else
            range 0 ((series |> String.length) - length)
            |> indexedMap (\idx _ -> series |> slice idx (idx + length))
            |> map product
            |> maximum
