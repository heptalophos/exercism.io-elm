module RnaTranscription exposing (toRNA)

import String exposing (cons, uncons, toList)
import Result exposing (map2)

toRNA : String -> Result Char String
toRNA dna =
    let 
        transcribe : Char -> Result Char Char
        transcribe nuc = 
            case nuc of
                'G' -> 
                    Ok 'C'
                'C' -> 
                    Ok 'G'
                'T' -> 
                    Ok 'A'
                'A' -> 
                    Ok 'U'
                _   -> 
                    Err nuc
    in
        case uncons dna of
            Just (head, rest) -> 
                map2 cons (transcribe head) (toRNA rest)
            Nothing -> 
                Ok ""