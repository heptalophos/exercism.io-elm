module TwoFer exposing (twoFer)

import Maybe exposing ( withDefault )
import String exposing( concat )


twoFer : Maybe String -> String
twoFer name =
    let
        who = withDefault "you" name
    in
        concat ["One for ", who, ", one for me."]
    
