module TisburyTreasureHunt exposing (..)

type alias TreasureLocation = ( Int, Char )
type alias PlaceLocation = ( Char, Int )
type alias Treasure = ( String, TreasureLocation )
type alias Place = ( String, PlaceLocation )



placeLocationToTreasureLocation : PlaceLocation -> TreasureLocation
placeLocationToTreasureLocation placeLocation =
    let
        (char, int) = placeLocation
    in 
        (int, char)


treasureLocationMatchesPlaceLocation : PlaceLocation -> TreasureLocation -> Bool
treasureLocationMatchesPlaceLocation placeLocation treasureLocation =
   placeLocation |> placeLocationToTreasureLocation
                 |> (==) treasureLocation


countPlaceTreasures : Place -> List Treasure -> Int
countPlaceTreasures place treasures =
    let 
        (_, pLoc) = place
    in
        treasures |> List.map (\(_, tLoc) -> tLoc)
        |> List.filter (pLoc |> treasureLocationMatchesPlaceLocation)
        |> List.length


specialCaseSwapPossible : ( String, TreasureLocation ) -> ( String, PlaceLocation ) -> ( String, TreasureLocation ) -> Bool
specialCaseSwapPossible ( foundTreasure, _ ) ( place, _ ) ( desiredTreasure, _ ) =
    case (foundTreasure, place, desiredTreasure) of
        ("Brass Spyglass", "Abandoned Lighthouse", _) -> 
            True
        ("Amethyst Octopus", "Stormy Breakwater", "Crystal Crab") -> 
            True
        ("Amethyst Octopus", "Stormy Breakwater", "Glass Starfish") -> 
            True
        ("Vintage Pirate Hat", "Harbor Managers Office", "Model Ship in Large Bottle") -> 
            True
        ("Vintage Pirate Hat", "Harbor Managers Office", "Antique Glass Fishnet Float") -> 
            True
        (_, _, _) -> 
            False
