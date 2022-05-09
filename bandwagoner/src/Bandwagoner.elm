module Bandwagoner exposing (..)


type alias Coach = 
    { name : String
    , formerPlayer : Bool
    }
type alias Stats = 
    { wins : Int
    , losses : Int 
    }
type alias Team = 
    { name : String
    , coach : Coach
    , stats : Stats
    }


replaceCoach : Coach -> Team -> Team
replaceCoach newCoach team =
    let 
        teamWithNewCoach = 
            { team | coach = newCoach }
    in
        teamWithNewCoach


createTeam : String -> Stats -> Coach -> Team
createTeam name stats coach =
    let 
        newTeam = { name = name
                  , coach = coach
                  , stats = stats 
                  }
    in  
        newTeam


rootForTeam : { any | stats : Stats } -> Bool
rootForTeam { stats } =
    stats.wins > stats.losses
