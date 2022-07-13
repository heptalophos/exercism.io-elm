module LuigisLusciousLasagna exposing (remainingTimeInMinutes)


remainingTimeInMinutes : Int -> Int -> Int
remainingTimeInMinutes numberOfLayers minutesAlreadyInOven =
    let
        standardCookingTime : Int
        standardCookingTime = 40

        preparation : Int -> Int
        preparation = (*) 2

        cooking : Int
        cooking = (-) standardCookingTime minutesAlreadyInOven
    in
        numberOfLayers |> preparation |> (+) cooking