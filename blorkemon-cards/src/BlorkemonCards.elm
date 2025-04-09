module BlorkemonCards exposing
    ( Card
    , compareShinyPower
    , expectedWinner
    , isMorePowerful
    , maxPower
    , sortByCoolness
    , sortByMonsterName
    )


type alias Card =
    { monster : String, power : Int, shiny : Bool }


isMorePowerful : Card -> Card -> Bool
isMorePowerful card1 card2 =
    card1.power > card2.power


maxPower : Card -> Card -> Int
maxPower card1 card2 =
    max card1.power card2.power


sortByMonsterName : List Card -> List Card
sortByMonsterName cards =
    List.sortBy (\c -> c.monster) cards


sortByCoolness : List Card -> List Card
sortByCoolness cards =
    let
        boolToInt truthValue =
            if truthValue then 1 else 0
    in
        List.sortBy (\c -> (1 - boolToInt c.shiny, -c.power)) cards
    


compareShinyPower : Card -> Card -> Order
compareShinyPower card1 card2 =
    let
        boolToInt truthValue =
            if truthValue then 1 else 0
    in
        case compare card1.power card2.power of
            EQ -> compare (boolToInt card1.shiny) (boolToInt card2.shiny)
            other -> other
                


expectedWinner : Card -> Card -> String
expectedWinner card1 card2 =
    case compareShinyPower card1 card2 of
        GT -> card1.monster
        LT -> card2.monster
        EQ -> "too close to call"
