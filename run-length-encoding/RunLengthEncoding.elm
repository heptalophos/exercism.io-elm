module RunLengthEncoding exposing (decode, encode, version)

import Char exposing (..)

version =
    2

stringifyNum : Int -> String
stringifyNum num = 
    if num > 1  then toString num
                else ""

encodeRec : String -> Int -> List String -> String
encodeRec lst num str = 
    case str of
        []        -> stringifyNum num ++ lst
        cur::rest -> if cur == lst then 
                        encodeRec lst (num + 1) rest
                     else
                        stringifyNum num ++ lst ++ encodeRec cur 1 rest
                  
decodeRec : String -> List Char -> String
decodeRec dig str = 
    case str of
        [] -> ""
        cur::rest -> if Char.isDigit cur then
                        decodeRec (dig ++ String.fromChar cur) rest
                     else
                        String.repeat
                            (Result.withDefault 1 (String.toInt dig)) 
                            (String.fromChar cur) ++ decodeRec "" rest

encode : String -> String
encode string = encodeRec "" 0 (string |> String.split "")

decode : String -> String
decode string = decodeRec "" (string |> String.toList)




