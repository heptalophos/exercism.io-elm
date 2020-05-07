module TwoFer exposing (twoFer)

import Maybe exposing ( withDefault )
import String exposing( concat )


twoFer : Maybe String -> String
twoFer name =
    concat ["One for ", withDefault "you" name, ", one for me."]
