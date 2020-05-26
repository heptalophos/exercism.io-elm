module PhoneNumber exposing (getNumber)


import String exposing (length, 
                        slice, 
                        filter, 
                        fromList, 
                        toList)
import Char   exposing (isDigit)
import Maybe  exposing (andThen)


getNumber : String -> Maybe String
getNumber =
    normalize
    >> andThen (validateNumber)
    >> andThen (validateDigitAt 0)
    >> andThen (validateDigitAt 3)


normalize : String -> Maybe String
normalize phoneNumber =
    let 
        digits = 
            phoneNumber |> filter isDigit
    in 
        case digits |> toList of
            '1' :: rest -> 
                Just (fromList rest)
            _ -> 
                Just digits 


validateNumber : String -> Maybe String
validateNumber phoneNumber =
    case length phoneNumber of
        10 -> 
            Just phoneNumber
        _  -> 
            Nothing


validateDigitAt : Int -> String -> Maybe String
validateDigitAt pos phoneNumber =
    case phoneNumber |> slice pos (pos + 1) of
        "0" -> 
            Nothing
        "1" -> 
            Nothing
        _   -> 
            Just phoneNumber
        