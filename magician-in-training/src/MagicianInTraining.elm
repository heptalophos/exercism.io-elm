module MagicianInTraining exposing (..)

import Array exposing ( Array )

getCard : Int -> Array Int -> Maybe Int
getCard = Array.get


setCard : Int -> Int -> Array Int -> Array Int
setCard = Array.set


addCard : Int -> Array Int -> Array Int
addCard  = Array.push


removeCard : Int -> Array Int -> Array Int
removeCard index deck =
    let 
        len = deck |> Array.length
        sl1 = deck |> Array.slice 0 index
        sl2 = deck |> Array.slice (index + 1) len
    in
        (sl1 |> Array.append) <| sl2 


evenCardCount : Array Int -> Int
evenCardCount =
    let
        even = (modBy 2) >> (==) 0
    in
        Array.filter even >> Array.length
