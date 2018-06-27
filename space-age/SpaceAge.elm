module SpaceAge exposing (Planet(..), ageOn)


type Planet
    = Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune

orbitalPeriod : Planet -> Float
orbitalPeriod planet =
    case planet of 
        Mercury -> 0.2408467 * 31557600
        Venus -> 0.61519726 * 31557600
        Earth -> 1.0 * 31557600
        Mars -> 1.8808158 * 31557600
        Jupiter -> 11.862615 * 31557600
        Saturn -> 29.447498 * 31557600
        Uranus -> 84.016846 * 31557600
        Neptune -> 164.79132 * 31557600


ageOn : Planet -> Float -> Float
ageOn planet seconds = 
        seconds
        |> flip (/) (orbitalPeriod planet)
    
