module Main exposing (main)

{-| You do not need to touch this file. Go to `src/App.elm` to paste in examples and fiddle around
-}

import Browser
import Chart
import Chart.Attributes
import Chart.Events
import Chart.Item
import Color
import FormatNumber
import FormatNumber.Locales
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Random
import Svg
import Svg.Attributes
import Time


type alias State =
    { label : String
    , valueName : String
    , goodValueMin : Float
    , goodValueMax : Float
    , observations : List Observation
    , hovering :
        List
            (Chart.Item.Item
                ( Chart.Item.One Observation Chart.Item.Any
                , List (Chart.Item.One Observation Chart.Item.Any)
                )
            )
    }


type alias Observation =
    { timestamp : Time.Posix
    , value : Float
    }


initialState : FlagsDecoded -> State
initialState flagsDecoded =
    { label = flagsDecoded.label
    , valueName = flagsDecoded.valueName
    , goodValueMin = flagsDecoded.goodValueMin
    , goodValueMax = flagsDecoded.goodValueMax
    , observations = flagsDecoded.observations
    , hovering = []
    }


view : State -> Html State
view state =
    uiChartFrame
        { label = state.label
        , valueName = state.valueName
        , goodValueMin = state.goodValueMin
        , goodValueMax = state.goodValueMax
        , hovering = state.hovering
        , observations = state.observations
        }
        |> Html.map
            (\newHovering ->
                { state
                    | hovering = newHovering
                }
            )



{- uiFrame []
   [  uiChartFrame
       { label = "Atemzüge/Minute in den letzten 6 Stunden"
       , chart =
           Chart.series
               (\observation -> observation.timestamp |> Time.posixToMillis |> Basics.toFloat)
               [ Chart.interpolated .breathCountPerMinute [] []
                   |> Chart.named "Atemzüge/Minute"
               ]
               (patientDetailsState.respiratoryRateHistory
                   |> List.sortBy
                       (\observation -> observation.timestamp |> Time.posixToMillis)
               )
       , goodValueMin = 21
       , goodValueMax = 26
       , hovering = patientDetailsState.respiratoryRateHovering
       }
       |> Html.map
           (\newHovering ->
               PatientDetailsState
                   { patientDetailsState
                       | respiratoryRateHovering = newHovering
                   }
           )
   , uiChartFrame
       { label = ""
       , chart =
           Chart.series
               (\observation -> observation.timestamp |> Time.posixToMillis |> Basics.toFloat)
               [ Chart.interpolated .degreesCelsius [] []
                   |> Chart.named ""
               ]
               (patientDetailsState.temperatureHistory
                   |> List.sortBy
                       (\observation -> observation.timestamp |> Time.posixToMillis)
               )
       , goodValueMin = 36.5
       , goodValueMax = 38
       , hovering = patientDetailsState.temperatureHovering
       }
       |> Html.map
           (\newHovering ->
               PatientDetailsState
                   { patientDetailsState
                       | temperatureHovering = newHovering
                   }
           )
   , uiChartFrame
       { label = ""
       , chart =
           Chart.series
               (\observation -> observation.timestamp |> Time.posixToMillis |> Basics.toFloat)
               [ Chart.interpolated (\obs -> obs.value |> feelingToPercentage) [] []
                   |> Chart.named ""
               ]
               (patientDetailsState.feelingHistory
                   |> List.sortBy
                       (\observation -> observation.timestamp |> Time.posixToMillis)
               )
       , goodValueMin = 0.48
       , goodValueMax = 1
       , hovering = patientDetailsState.feelingHovering
       }
       |> Html.map
           (\newHovering ->
               PatientDetailsState
                   { patientDetailsState
                       | feelingHovering = newHovering
                   }
           )
   ]
-}


type alias FlagsDecoded =
    { label : String
    , valueName : String
    , goodValueMin : Float
    , goodValueMax : Float
    , observations : List Observation
    }


flagsDecoder : Json.Decode.Decoder FlagsDecoded
flagsDecoder =
    Json.Decode.map5
        (\label valueName goodValueMin goodValueMax observations ->
            { label = label
            , valueName = valueName
            , goodValueMin = goodValueMin
            , goodValueMax = goodValueMax
            , observations = observations
            }
        )
        (Json.Decode.field "label" Json.Decode.string)
        (Json.Decode.field "valueName" Json.Decode.string)
        (Json.Decode.field "goodValueMin"
            (Json.Decode.andThen
                (\string ->
                    case string |> String.toFloat of
                        Nothing ->
                            Json.Decode.fail "not a float"

                        Just float ->
                            Json.Decode.succeed float
                )
                Json.Decode.string
            )
        )
        (Json.Decode.field "goodValueMax"
            (Json.Decode.andThen
                (\string ->
                    case string |> String.toFloat of
                        Nothing ->
                            Json.Decode.fail "not a float"

                        Just float ->
                            Json.Decode.succeed float
                )
                Json.Decode.string
            )
        )
        (Json.Decode.field "observations"
            (Json.Decode.map
                (\string ->
                    string
                        |> String.split " "
                        |> List.filterMap
                            (\observationString ->
                                case observationString |> String.split ":" of
                                    [ timestampString, valueString ] ->
                                        case
                                            ( Maybe.map Time.millisToPosix
                                                (String.toInt timestampString)
                                            , String.toFloat valueString
                                            )
                                        of
                                            ( Just timestamp, Just value ) ->
                                                Just { timestamp = timestamp, value = value }

                                            _ ->
                                                -- Json.Decode.fail "observation must be in the form posix-millis:value"
                                                Nothing

                                    _ ->
                                        -- Json.Decode.fail "observation must be in the form posix-millis:value"
                                        Nothing
                            )
                )
                Json.Decode.string
            )
        )


uiChartFrame :
    { label : String
    , hovering : List (Chart.Item.Item b)
    , valueName : String
    , goodValueMin : Float
    , goodValueMax : Float
    , observations : List Observation
    }
    ->
        Html
            (List
                (Chart.Item.Item
                    ( Chart.Item.One Observation Chart.Item.Any
                    , List (Chart.Item.One Observation Chart.Item.Any)
                    )
                )
            )
uiChartFrame config =
    Html.div
        []
        [ Html.label
            [ Html.Attributes.style "font-size" "1rem"
            ]
            [ Html.h4
                [ Html.Attributes.style "margin-bottom" "0px"
                ]
                [ Html.text config.label ]
            ]
        , Html.div
            [ Html.Attributes.style "max-width" "500px"
            , Html.Attributes.style "min-width" "500px"
            , Html.Attributes.style "padding" "20px 50px 20px 50px"
            ]
            [ Chart.chart
                [ Chart.Attributes.width 500
                , Chart.Attributes.height 400
                , Chart.Events.onMouseMove identity (Chart.Events.getNearest Chart.Item.bins)
                , Chart.Events.onMouseLeave []
                ]
                [ Chart.withPlane <|
                    \p ->
                        [ Chart.rect
                            [ Chart.Attributes.x1 p.x.min
                            , Chart.Attributes.y1
                                config.goodValueMin
                            , Chart.Attributes.x2 p.x.max
                            , Chart.Attributes.y2
                                (Basics.min
                                    config.goodValueMax
                                    (config.observations
                                        |> List.map .value
                                        |> List.maximum
                                        |> Maybe.withDefault config.goodValueMax
                                    )
                                )
                            , Chart.Attributes.opacity 0.1
                            , Chart.Attributes.border "none"
                            , Chart.Attributes.color Chart.Attributes.green
                            ]
                        ]
                , Chart.labelAt Chart.Attributes.middle
                    .min
                    [ Chart.Attributes.moveUp 10 ]
                    [ Svg.text "Zeitabstand bis jetzt" ]
                , Chart.grid
                    [ Chart.Attributes.color
                        (Color.black |> Color.toCssString)
                    ]
                , Chart.xTicks
                    [ Chart.Attributes.times Time.utc
                    , Chart.Attributes.noGrid
                    ]
                , Chart.yTicks
                    [ Chart.Attributes.withGrid
                    ]
                , Chart.xLabels
                    [ Chart.Attributes.times Time.utc
                    , Chart.Attributes.color (Color.black |> Color.toCssString)
                    , Chart.Attributes.moveDown 5
                    ]
                , Chart.yLabels [ Chart.Attributes.color (Color.black |> Color.toCssString) ]
                , Chart.xAxis [ Chart.Attributes.color (Color.black |> Color.toCssString) ]
                , Chart.yAxis [ Chart.Attributes.color (Color.black |> Color.toCssString) ]
                , Chart.series
                    (\observation ->
                        observation.timestamp |> Time.posixToMillis |> Basics.toFloat
                    )
                    [ Chart.interpolated .value [] []
                        |> Chart.named config.valueName
                    ]
                    (config.observations
                        |> List.sortBy
                            (\observation -> observation.timestamp |> Time.posixToMillis)
                    )
                , Chart.each config.hovering
                    (\_ item ->
                        [ Chart.tooltip item [] [] [] ]
                    )
                ]
            ]
        ]


main : Platform.Program Json.Decode.Value State State
main =
    Browser.element
        { init =
            \json ->
                case json |> Json.Decode.decodeValue flagsDecoder of
                    Err error ->
                        ( { label =
                                "invalid data attributes: "
                                    ++ Json.Decode.errorToString error
                          , valueName = ""
                          , goodValueMin = 0
                          , goodValueMax = 0
                          , observations = []
                          , hovering = []
                          }
                        , Cmd.none
                        )

                    Ok flagsDecoded ->
                        ( initialState flagsDecoded, Cmd.none )
        , view = view
        , update = \newState _ -> ( newState, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
