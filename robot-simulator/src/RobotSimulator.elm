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
    { bearing : Bearing, 
      coordinates : { x : Int, y : Int } }


defaultRobot : Robot
defaultRobot =
    { bearing = North, 
      coordinates = { x = 0, y = 0 } }


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
            North -> 
                Robot North { y = y + 1, x = x }
            East  -> 
                Robot East  { x = x + 1, y = y }
            South -> 
                Robot South { y = y - 1, x = x }
            West  -> 
                Robot West  { x = x - 1, y = y }


simulate : String -> Robot -> Robot
simulate directions robot =
    directions 
    |> toList 
    |> foldl execute robot


-- auxiliary fcns

-- Convert the instructions in a program, 
-- to actions
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


-- A generic turn function : find the current bearing 
-- in a list of bearings (which serves as a sequence 
-- of orientations, expressing a turning rule-set) and 
-- assume the next bearing in the list.
turn : Bearing -> List Bearing -> Bearing
turn bearing orientations =
    case orientations of 
        d1 :: d2 :: tail ->
            case d1 == bearing of 
                True -> 
                        d2
                _ -> 
                    turn bearing (d2 :: tail)
        _ -> 
            North


-- The clockwise-by-90-degrees turns sequence 
clockwise : List Bearing
clockwise = 
    [North, East, South, West]


-- And the counterclockwise one 
counterClockwise : List Bearing
counterClockwise = 
    [North, West, South, East]