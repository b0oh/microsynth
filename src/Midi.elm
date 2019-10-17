module Midi exposing
    ( Note
    , keyToMidi
    , midiToFreq
    , midiToKey
    )


type alias Note =
    Int


keyToMidi key =
    case key of
        "a" -> Just 60
        "w" -> Just 61
        "s" -> Just 62
        "e" -> Just 63
        "d" -> Just 64
        "f" -> Just 65
        "t" -> Just 66
        "g" -> Just 67
        "y" -> Just 68
        "h" -> Just 69
        "u" -> Just 70
        "j" -> Just 71
        "k" -> Just 72
        "o" -> Just 73
        "l" -> Just 74
        "p" -> Just 75
        ";" -> Just 76
        _ -> Nothing


midiToFreq : Note -> Float
midiToFreq note =
    let
        ( baseNote, baseFreq ) =
            ( 69.0, 440.0 )

        power =
            (toFloat note - baseNote) / 12
    in
    (2.0 ^ power) * baseFreq


midiToKey midi =
    case midi of
        60 -> Just "a"
        61 -> Just "w"
        62 -> Just "s"
        63 -> Just "e"
        64 -> Just "d"
        65 -> Just "f"
        66 -> Just "t"
        67 -> Just "g"
        68 -> Just "y"
        69 -> Just "h"
        70 -> Just "u"
        71 -> Just "j"
        72 -> Just "k"
        73 -> Just "o"
        74 -> Just "l"
        75 -> Just "p"
        76 -> Just ";"
        _ -> Nothing
