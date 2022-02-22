module Isogram exposing (isIsogram)


isIsogram : String -> Bool
isIsogram sentence =
    let
        unique : List Char -> Bool
        unique chars =
            case chars of
                [] -> True 
                x::[] -> True
                x::y::xs -> x /= y && unique (y::xs)    
    in
        String.toLower
        >> String.toList
        >> List.filter(\c -> c >= 'a' && c <= 'z')
        >> List.sort
        >> unique
        <| sentence
