module MazeMaker exposing (..)

import Random exposing (Generator)


type Maze
    = DeadEnd
    | Room Treasure
    | Branch (List Maze)


type Treasure
    = Gold
    | Diamond
    | Friendship


deadend : Generator Maze
deadend =
    -- Random.lazy (\_ -> Debug.todo "Please implement deadend")
    Random.constant DeadEnd

treasure : Generator Treasure
treasure =
    -- Random.lazy (\_ -> Debug.todo "Please implement treasure")
    Random.uniform Gold [ Diamond, Friendship ]


room : Generator Maze
room =
    -- Random.lazy (\_ -> Debug.todo "Please implement room")
    Random.map Room treasure


branch : Generator Maze -> Generator Maze
branch mazeGenerator =
    -- Random.lazy (\_ -> Debug.todo "Please implement branch")
    Random.int 2 4
        |> Random.andThen (Random.list >> (|>) mazeGenerator)
        |> Random.map Branch


maze : Generator Maze
maze =
    -- Random.lazy (\_ -> Debug.todo "Please implement maze")
    Random.weighted 
        ( 12, deadend )
        [ ( 3, room )
        , ( 5, (\_ -> maze) |> Random.lazy |> branch )
        ]
        |> Random.andThen identity


mazeOfDepth : Int -> Generator Maze
mazeOfDepth depth =
    -- Random.lazy (\_ -> Debug.todo "Please implement mazeOfDepth")
    if depth <= 0 then
        -- deadend
        Random.uniform deadend [ room ]
        |> Random.andThen identity
    else
        mazeOfDepth (depth - 1) |> branch