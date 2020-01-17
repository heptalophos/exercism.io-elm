module RobotSimulator exposing
    ( Bearing(..)
    , Robot
    , advance
    , defaultRobot
    , simulate
    , turnLeft
    , turnRight
    )


import String exposing (toList)
import List exposing (foldl)


type Bearing
    = North | East | South | West


type alias Robot =
    { bearing : Bearing, coordinates : { x : Int, y : Int } }


defaultRobot : Robot
defaultRobot =
    { bearing = North, coordinates = { x = 0, y = 0 } }


turnRight : Robot -> Robot
turnRight robot =
    let 
        rightFace = turn robot.bearing clockwise
        samePosition = robot.coordinates
    in 
        Robot rightFace samePosition


turnLeft : Robot -> Robot
turnLeft robot =
    let 
        leftFace = turn robot.bearing counterClockwise
        samePosition = robot.coordinates
    in 
        Robot leftFace samePosition


advance : Robot -> Robot
advance robot =
    let 
        {x, y} = robot.coordinates 
    in 
        case robot.bearing of
            North -> Robot North { y = y + 1, x = x }
            East  -> Robot East  { x = x + 1, y = y }
            South -> Robot South { y = y - 1, x = x }
            West  -> Robot West  { x = x - 1, y = y }


simulate : String -> Robot -> Robot
simulate directions robot =
    directions |> toList |> foldl execute robot


-- auxiliary

execute : Char -> Robot -> Robot
execute instruction robot =
    case instruction of
        'R' -> 
            robot |> turnRight 
        'L' -> 
            robot |> turnLeft 
        'A' -> 
            robot |> advance
        _   -> 
            robot |> identity


turn : Bearing -> List Bearing -> Bearing
turn bearing directions =
    case directions of 
        d1 :: d2 :: tail ->
            case d1 == bearing of 
                True -> d2
                _    -> turn bearing (d2 :: tail)
        _ -> North


clockwise : List Bearing
clockwise = [North, East, South, West]


counterClockwise : List Bearing
counterClockwise = [North, West, South, East]