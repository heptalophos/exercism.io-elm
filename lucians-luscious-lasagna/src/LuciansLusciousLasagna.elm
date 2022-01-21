module LuciansLusciousLasagna exposing 
    ( elapsedTimeInMinutes, 
      expectedMinutesInOven, 
      preparationTimeInMinutes )


expectedMinutesInOven : Int 
expectedMinutesInOven = 40


preparationTimeInMinutes : Int -> Int
preparationTimeInMinutes = (*) 2


elapsedTimeInMinutes : Int -> Int -> Int
elapsedTimeInMinutes layers time =
    layers |> preparationTimeInMinutes |> (+) time
