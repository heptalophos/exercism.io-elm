module TicketPlease exposing (..)

import TicketPleaseSupport exposing (Status(..), Ticket(..), User(..))


emptyComment : ( User, String ) -> Bool
emptyComment comment =
    case comment of
        (_, "") -> True
        _       -> False
    


numberOfCreatorComments : Ticket -> Int
numberOfCreatorComments (Ticket ticket) =
    let 
        (creator, _) = ticket.createdBy
        count author (user, _) = if author == user then (+) 1 else (+) 0
    in
        List.foldl (count (Tuple.first ticket.createdBy)) 
                   0 
                   ticket.comments


assignedToDevTeam : Ticket -> Bool
assignedToDevTeam (Ticket ticket) =
    case ticket.assignedTo of
        Just (User username) ->
            case username of
                "Alice"   -> True
                "Bob"     -> True
                "Charlie" -> True
                _         -> False
        Nothing ->
            False



assignTicketTo : User -> Ticket -> Ticket
assignTicketTo user (Ticket ticket) =
    case ticket.status of
        New      ->
            Ticket { ticket | assignedTo = Just user, status = InProgress }
        Archived -> 
            Ticket ticket
        _        ->
            Ticket { ticket | assignedTo = Just user }
