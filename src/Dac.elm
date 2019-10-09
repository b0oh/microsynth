port module Dac exposing
    ( Sample
    , queueSamples
    , requestSamples
    , sampleRate
    , start
    , stop
    )


type alias Sample =
    Float


port dacQueueSamples : List Sample -> Cmd msg
port dacRequestSamples : (Int -> msg) -> Sub msg
port dacStart : () -> Cmd msg
port dacStop : () -> Cmd msg


queueSamples =
    dacQueueSamples


requestSamples =
    dacRequestSamples


sampleRate =
    44100


start =
    dacStart ()


stop =
    dacStop ()
