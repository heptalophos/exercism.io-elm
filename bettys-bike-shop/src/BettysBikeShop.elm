module BettysBikeShop exposing (penceToPounds, poundsToString)


import String


penceToPounds : Int -> Float
penceToPounds =
    toFloat >> (*) 0.01


poundsToString : Float -> String
poundsToString =
    String.fromFloat >> (++) "£"
