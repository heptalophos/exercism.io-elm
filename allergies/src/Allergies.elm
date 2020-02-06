module Allergies exposing (Allergy(..), 
                           isAllergicTo, 
                           toList)

import List exposing (filter)
import Bitwise exposing (and)


type Allergy
    = Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats

isAllergicTo : Allergy -> Int -> Bool
isAllergicTo allergy score =
    allergy 
    |> allergyValue 
    |> and score >> (/=) 0


toList : Int -> List Allergy
toList score =
    allergies 
    |> filter (flip isAllergicTo score) 

-- Auxiliary

allergies : List Allergy
allergies = [ Eggs, 
              Peanuts,
              Shellfish,
              Strawberries,
              Tomatoes, 
              Chocolate,
              Pollen,
              Cats ] 


allergyValue : Allergy -> Int
allergyValue allergy =
    case allergy of
        Eggs -> 
            1  
        Peanuts -> 
            2
        Shellfish -> 
            4 
        Strawberries -> 
            8 
        Tomatoes -> 
            16
        Chocolate -> 
            32
        Pollen -> 
            64
        Cats-> 
            128

flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a