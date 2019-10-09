module Main exposing (main)

import Browser
import Dac exposing (Sample)
import Json.Decode as Json
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Time


type alias State =
    { bufferSize : Int
    , bufferSizeInput : String
    , play : Bool
    }


type Message
    = OnBufferSizeChange String
    | ReadBufferSize
    | RequestSamples Int
    | ToggleSound


defaultState =
    { bufferSize = 25
    , bufferSizeInput = "25"
    , play = False
    }


init flags =
    ( defaultState
    , Cmd.none
    )


freq =
    440


sampleFreq =
    toFloat Dac.sampleRate / freq


sample : Int -> Sample
sample sampleNumber =
    sin (toFloat sampleNumber / (sampleFreq / (pi * 2)))


makeSamples : Int -> Int -> List Sample
makeSamples amount sampleNumber =
    let
        start =
            sampleNumber

        end =
            start + amount - 1

        samples =
            List.range start end
                |> List.map sample
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
                    makeSamples samplesAmount sampleNumber
            in
            ( state
            , Dac.queueSamples samples
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


subscriptions : State -> Sub Message
subscriptions state =
    Sub.batch
        [ Dac.requestSamples RequestSamples
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
