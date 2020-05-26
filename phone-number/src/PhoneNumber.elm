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
    >> andThen (validateDigit 0)
    >> andThen (validateDigit 3)


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


validateDigit : Int -> String -> Maybe String
validateDigit atIndex phoneNumber =
    case phoneNumber |> slice atIndex (atIndex + 1) of
        "0" -> 
            Nothing
        "1" -> 
            Nothing
        _   -> 
            Just phoneNumber
        