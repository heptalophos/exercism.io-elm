module LuigisLusciousLasagna exposing (remainingTimeInMinutes)


remainingTimeInMinutes : Int -> Int -> Int
remainingTimeInMinutes numberOfLayers minutesAlreadyInOven =
    let
        standardCookingTime : Int
        standardCookingTime = 40

        preparationTime : Int -> Int
        preparationTime layers = 2 * layers
    in
        preparationTime numberOfLayers +
        standardCookingTime - minutesAlreadyInOven
