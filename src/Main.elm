module Main exposing (main)

import Browser
import Browser.Events
import Dac exposing (Sample)
import Json.Decode as Json
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Midi
import Time


type alias Model =
    { bufferSize : Int
    , bufferSizeInput : String
    , note : Maybe Midi.Note
    , running : Bool
    }


type Message
    = OnBufferSizeChange String
    | ReadBufferSize
    | RequestSamples Int
    | NoteOn Midi.Note
    | NoteRelease Midi.Note
    | SetBufferSize String
    | Start
    | Stop


defaultModel =
    { bufferSize = 25
    , bufferSizeInput = "25"
    , note = Nothing
    , running = True
    }


init flags =
    let
        command =
            if defaultModel.running then
                Dac.start

            else
                Dac.stop
    in
    ( defaultModel
    , command
    )


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


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        OnBufferSizeChange input ->
            ( { model | bufferSizeInput = input }
            , Cmd.none
            )

        ReadBufferSize ->
            case String.toInt model.bufferSizeInput of
                Just size ->
                    ( { model | bufferSize = size }
                    , Cmd.none
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        RequestSamples sampleNumber ->
            let
                samplesAmount =
                    round (toFloat (Dac.sampleRate * model.bufferSize) / 1000)

                freq =
                    model.note
                        |> Maybe.map (Midi.midiToFreq)
                        |> Maybe.withDefault 0

                samples =
                    makeSamples freq samplesAmount sampleNumber
            in
            ( model
            , Dac.queueSamples samples
            )

        NoteOn note ->
            ( { model | note = Just note }
            , Cmd.none
            )

        NoteRelease note ->
            let
                newModel =
                    if model.note == Just note then
                        { model | note = Nothing }

                    else
                        model
            in
            ( newModel
            , Cmd.none
            )

        SetBufferSize string ->
            let
                newModel =
                    case String.toInt string of
                        Just size ->
                            { model
                                | bufferSize = size
                                , bufferSizeInput = String.fromInt size
                            }

                        _ ->
                            model
            in
            ( newModel
            , Cmd.none
            )

        Start ->

            ( { model | running = True }
            , Dac.start
            )

        Stop ->
            ( { model | running = False }
            , Dac.stop
            )


midiEvent toMessage =
    Json.field "key" Json.string
        |> Json.andThen
           (\key ->
                case Midi.keyToMidi key of
                    Just note ->
                        Json.succeed <| toMessage note

                    _ ->
                        Json.fail ""
           )


onPress =
    midiEvent NoteOn
        |> Browser.Events.onKeyPress


onRelease =
    midiEvent NoteRelease
        |> Browser.Events.onKeyUp


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ Dac.requestSamples RequestSamples
        , onPress
        , onRelease
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


view : Model -> Html Message
view model =
    let
        toggle =
            if model.running then
                Html.button
                    [ Events.onClick Stop ]
                    [ Html.text "stop" ]

            else
                Html.button
                    [ Events.onClick Start ]
                    [ Html.text "start" ]

        bufferLabel =
            Html.text "Buffer (ms): "

        bufferInput =
            Html.input
                [ onEnter ReadBufferSize
                , Attrs.value model.bufferSizeInput
                , Events.onInput OnBufferSizeChange
                ]
                []

        bufferSubmit =
            Html.button
                [ Events.onClick ReadBufferSize ]
                [ Html.text "set" ]

        bufferRange =
            Html.input
                [ Attrs.type_ "range"
                , Attrs.min "0"
                , Attrs.max "250"
                , Attrs.value <| String.fromInt model.bufferSize
                , Events.onInput SetBufferSize
                ]
                []

        hromatic midiNote =
            let
                note =
                    modBy 12 midiNote
            in
            List.member note [ 1, 3, 6, 8, 10 ]

        key note =
            let
                varying =
                    if model.note == Just note then
                        [ Attrs.style "background" "blue"
                        , Attrs.style "color" "white"
                        ]

                    else
                        if hromatic note then
                            [ Attrs.style "background" "black"
                            , Attrs.style "color" "white"
                            ]

                        else
                            [ Attrs.style "background" "white"
                            , Attrs.style "color" "black"
                            ]

                styles =
                    varying ++
                    [ Attrs.style "border-right" "2px solid lightgray"
                    , Attrs.style "display" "inline-block"
                    , Attrs.style "height" "200px"
                    , Attrs.style "line-height" "200px"
                    , Attrs.style "text-align" "center"
                    , Attrs.style "width" "50px"
                    , Mouse.onDown (always (NoteOn note))
                    , Mouse.onUp (always (NoteRelease note))
                    , Touch.onStart (always (NoteOn note))
                    , Touch.onEnd (always (NoteRelease note))
                    ]

                keyboardKey =
                    Midi.midiToKey note
                        |> Maybe.withDefault ""
            in
            Html.div
                styles
                [ Html.text keyboardKey
                ]

        keys =
            List.range 60 76
                |> List.map key
                |> Html.div
                   [ Attrs.style "border" "2px solid lightgray"
                   , Attrs.style "border-width" "2px 0px 2px 2px"
                   , Attrs.style "display" "inline-block"
                   ]

        keyboard =
            keys
    in
    Html.div
        []
        [ toggle
        , Html.div
            []
            [ Html.div
                  []
                  [ bufferLabel
                  , bufferInput
                  , bufferSubmit
                  ]
            , bufferRange
            ]
        , keyboard
        ]


main : Program () Model Message
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
