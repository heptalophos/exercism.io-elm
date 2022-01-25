module BettysBikeShop exposing (penceToPounds, poundsToString)


import String


penceToPounds : Int -> Float
penceToPounds pence =
    (*) (pence |> toFloat) 0.01


poundsToString : Float -> String
poundsToString pounds =
    (++) "£" (pounds |> String.fromFloat) 
