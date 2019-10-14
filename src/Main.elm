module Main exposing (main)

import Browser
import Browser.Events
import Dac exposing (Sample)
import Json.Decode as Json
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Time


type alias State =
    { bufferSize : Int
    , bufferSizeInput : String
    , freq : Float
    , play : Bool
    }


type Message
    = OnBufferSizeChange String
    | ReadBufferSize
    | RequestSamples Int
    | SetFreq Float
    | ToggleSound


defaultState =
    { bufferSize = 25
    , bufferSizeInput = "25"
    , freq = 440
    , play = False
    }


init flags =
    ( defaultState
    , Cmd.none
    )


type Note
    = C
    | D
    | E


sampleFreq freq =
    toFloat Dac.sampleRate / freq


sample : Float -> Int -> Sample
sample freq sampleNumber =
    sin (toFloat sampleNumber / (sampleFreq freq / (pi * 2)))


makeSamples : Float -> Int -> Int -> List Sample
makeSamples freq amount sampleNumber =
    let
        start =
            sampleNumber

        end =
            start + amount - 1

        samples =
            List.range start end
                |> List.map (sample freq)
    in
    samples


update : Message -> State -> ( State, Cmd Message )
update message state =
    case message of
        OnBufferSizeChange input ->
            ( { state | bufferSizeInput = input }
            , Cmd.none
            )

        ReadBufferSize ->
            case String.toInt state.bufferSizeInput of
                Just size ->
                    ( { state | bufferSize = size }
                    , Cmd.none
                    )

                Nothing ->
                    ( state
                    , Cmd.none
                    )

        RequestSamples sampleNumber ->
            let
                samplesAmount =
                    round (toFloat (Dac.sampleRate * state.bufferSize) / 1000)

                samples =
                    makeSamples state.freq samplesAmount sampleNumber
            in
            ( state
            , Dac.queueSamples samples
            )

        SetFreq freq ->
            ( { state | freq = freq }
            , Cmd.none
            )

        ToggleSound ->
            if state.play then
                ( { state | play = False }
                , Dac.stop
                )

            else
                ( { state | play = True }
                , Dac.start
                )


charToNote char =
    if char == "a" then
        Just C

    else if char =="s" then
        Just D

    else if char == "d" then
        Just E

    else
        Nothing


noteToFreq note =
    if note == C then
        523.251

    else if note == D then
        587.330

    else if note == E then
        659.255

    else
        0


charToFreq char =
    charToNote char
        |> Maybe.map noteToFreq


keyboard =
    Browser.Events.onKeyDown
        (Json.field "key" Json.string
            |> Json.andThen
                 (\key ->
                      case charToFreq key of
                          Just freq ->
                              Json.succeed <| SetFreq freq

                          _ ->
                              Json.fail ""
                 )
        )


subscriptions : State -> Sub Message
subscriptions state =
    Sub.batch
        [ Dac.requestSamples RequestSamples
        , keyboard
        ]


onEnter msg =
    Events.on "keyup"
        (Json.field "key" Json.string
            |> Json.andThen
                 (\key ->
                      if key == "Enter" then
                          Json.succeed msg

                      else
                          Json.fail "Not the enter key"
                 )
        )


view : State -> Html Message
view state =
    let
        toggle =
            Html.button
                [ Events.onClick ToggleSound ]
                [ Html.text (if state.play then "stop" else "play") ]

        bufferInput =
            Html.input
                [ onEnter ReadBufferSize
                , Attrs.value state.bufferSizeInput
                , Events.onInput OnBufferSizeChange
                ]
                []

        bufferSubmit =
            Html.button
                [ Events.onClick ReadBufferSize ]
                [ Html.text "set" ]
    in
    Html.div
        []
        [ toggle
        , Html.div
            []
            [ bufferInput
            , bufferSubmit
            ]
        ]


main : Program () State Message
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
