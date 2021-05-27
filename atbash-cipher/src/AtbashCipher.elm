module AtbashCipher exposing (decode, encode)

chunkify : Int -> String -> String
chunkify n text =
    case text of 
        "" -> 
            text
        _  -> 
            (String.left n text) ++ 
            " " ++ 
            (chunkify n (String.dropLeft n text))

atbash : Char -> Char 
atbash ch =
    if Char.isAlpha ch then
        Char.fromCode((Char.toCode 'a') + 
        (Char.toCode 'z') - Char.toCode ch)
    else 
        ch


encode : String -> String
encode plain = 
    plain 
    |> String.toLower
    |> String.filter (\c -> Char.isLower c || Char.isDigit c)
    |> String.map atbash
    |> chunkify 5
    |> String.trim


decode : String -> String
decode cipher = 
    cipher
    |> String.filter (\c -> Char.isLower c || Char.isDigit c)
    |> String.map atbash
