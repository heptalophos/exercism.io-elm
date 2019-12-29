module PhoneNumber exposing (getNumber)

import String exposing (uncons, length, slice, filter, fromList, toList)
import Char exposing (isDigit)
import Maybe exposing (andThen)

getNumber : String -> Maybe String
getNumber phoneNumber = 
    phoneNumber
    |> normalize
    |> andThen (validateNumber)
    |> andThen (validateDigitAt 0)
    |> andThen (validateDigitAt 3)


normalize : String -> Maybe String
normalize string =
    let 
        digits = string |> filter isDigit
    in 
        case digits |> toList of
            '1' :: tail -> Just (fromList tail)
            _ -> Just digits 

validateNumber : String -> Maybe String
validateNumber string =
    case length string of
        10 -> Just string
        _  -> Nothing

validateDigitAt : Int -> String -> Maybe String
validateDigitAt position string =
    case string |> slice position (position + 1) of
        "0" -> Nothing
        "1" -> Nothing
        _   -> Just string