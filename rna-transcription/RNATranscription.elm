module RNATranscription exposing (toRNA)

import String exposing (cons, toList)

toRNA : String -> Result Char String
toRNA dna =
    let 
        transcribe : Char -> Result Char Char
        transcribe nuc = 
            case nuc of
                'G' -> Ok 'C'
                'C' -> Ok 'G'
                'T' -> Ok 'A'
                'A' -> Ok 'U'
                _   -> Err nuc
    in
        case String.uncons dna of
            Just (hd, tl) -> Result.map2 cons (transcribe hd) (toRNA tl)
            Nothing       -> Ok ""

