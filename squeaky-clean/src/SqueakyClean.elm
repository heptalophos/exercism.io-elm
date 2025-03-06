module SqueakyClean exposing (clean, clean1, clean2, clean3, clean4)
import String exposing (replace, map, filter)


clean1 : String -> String
clean1 =
    replace " " "_"

clean2 : String -> String
clean2 =
    let
        replaceCtrls = 
            replace "\n" "[CTRL]" <<
            map (\x -> if (Char.toCode x) < 32 then '\n' else x)
    in 
        clean1 >> replaceCtrls

clean3 : String -> String
clean3 =
    let
        toCamel = replace "-ḃ" "Ḃ" >> replace "-a" "A"
    in 
        clean2 >> toCamel

clean4 : String -> String
clean4 =
    let 
        removeDigits = filter (\c -> c |> Char.isDigit |> not)
    in
        clean3 >> removeDigits 

clean : String -> String
clean =
    let
        removeGreeks = filter (\char -> char < 'α' || char > 'ω')
    in 
        clean4 >> removeGreeks 
