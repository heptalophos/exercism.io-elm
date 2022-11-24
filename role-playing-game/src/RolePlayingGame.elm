module RolePlayingGame exposing (Player, castSpell, introduce, revive)


type alias Player =
    { name : Maybe String
    , level : Int
    , health : Int
    , mana : Maybe Int
    }


introduce : Player -> String
introduce { name } =
    name |> Maybe.withDefault "Mighty Magician" 


revive : Player -> Maybe Player
revive player =
    if player.health == 0 then
        case player.level > 9 of
            True -> 
                Just { name = player.name
                     , level = player.level
                     , health = 100
                     , mana = Just 100 
                     }
            _ -> 
                Just { name = player.name
                     , level = player.level
                     , health = 100
                     , mana = Nothing 
                     }
    else
        Nothing


castSpell : Int -> Player -> ( Player, Int )
castSpell manaCost player =
    case Maybe.map ((<=) manaCost) player.mana of
        Just True -> 
            ( { name = player.name
              , level = player.level
              , health = player.health
              , mana = player.mana 
                       |> Maybe.map (((+) -manaCost) >> max 0) 
              }
            , (*) 2 manaCost
            )
        Just _ -> 
            ( player, 0 )
        Nothing ->
            ( { name = player.name
              , level =player.level
              , health = player.health - manaCost |> max 0 
              , mana = player.mana
              }
            , 
              0
            )
