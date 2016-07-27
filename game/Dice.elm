module Dice exposing (..)

import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Random


type alias Model =
    Int


init : ( Model, Cmd Msg )
init =
    ( 1, Cmd.none )


type Msg
    = Roll
    | Set Int


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update msg model =
    case msg of
        Roll ->
            ( model, Random.generate Set (Random.int 1 6) )

        Set value ->
            ( value, Cmd.none )


view model =
    viewRender [ onClick Roll ] model "white"


viewRender attributes model fillColor =
    case model of
        1 ->
            svg attributes
                [ rect [ rx "4", cy "4", width "100", height "100", rx "7", fill fillColor, stroke "black", strokeWidth "8" ] []
                , circle [ cx "50", cy "50", r "10", fill (getPointsColor fillColor) ] []
                ]

        2 ->
            svg attributes
                [ rect [ rx "4", cy "4", width "100", height "100", rx "7", fill fillColor, stroke "black", strokeWidth "8" ] []
                , circle [ cx "30", cy "50", r "10", fill (getPointsColor fillColor) ] []
                , circle [ cx "70", cy "50", r "10", fill (getPointsColor fillColor) ] []
                ]

        3 ->
            svg attributes
                [ rect [ rx "4", cy "4", width "100", height "100", rx "7", fill fillColor, stroke "black", strokeWidth "8" ] []
                , circle [ cx "25", cy "50", r "10", fill (getPointsColor fillColor) ] []
                , circle [ cx "50", cy "50", r "10", fill (getPointsColor fillColor) ] []
                , circle [ cx "75", cy "50", r "10", fill (getPointsColor fillColor) ] []
                ]

        4 ->
            svg attributes
                [ rect [ rx "4", cy "4", width "100", height "100", rx "7", fill fillColor, stroke "black", strokeWidth "8" ] []
                , circle [ cx "30", cy "30", r "10", fill (getPointsColor fillColor) ] []
                , circle [ cx "70", cy "30", r "10", fill (getPointsColor fillColor) ] []
                , circle [ cx "30", cy "70", r "10", fill (getPointsColor fillColor) ] []
                , circle [ cx "70", cy "70", r "10", fill (getPointsColor fillColor) ] []
                ]

        5 ->
            svg attributes
                [ rect [ rx "4", cy "4", width "100", height "100", rx "7", fill fillColor, stroke "black", strokeWidth "8" ] []
                , circle [ cx "30", cy "30", r "10", fill (getPointsColor fillColor) ] []
                , circle [ cx "70", cy "30", r "10", fill (getPointsColor fillColor) ] []
                , circle [ cx "50", cy "50", r "10", fill (getPointsColor fillColor) ] []
                , circle [ cx "30", cy "70", r "10", fill (getPointsColor fillColor) ] []
                , circle [ cx "70", cy "70", r "10", fill (getPointsColor fillColor) ] []
                ]

        6 ->
            svg attributes
                [ rect [ rx "4", cy "4", width "100", height "100", rx "7", fill fillColor, stroke "black", strokeWidth "8" ] []
                , circle [ cx "25", cy "30", r "10", fill (getPointsColor fillColor) ] []
                , circle [ cx "50", cy "30", r "10", fill (getPointsColor fillColor) ] []
                , circle [ cx "75", cy "30", r "10", fill (getPointsColor fillColor) ] []
                , circle [ cx "25", cy "70", r "10", fill (getPointsColor fillColor) ] []
                , circle [ cx "50", cy "70", r "10", fill (getPointsColor fillColor) ] []
                , circle [ cx "75", cy "70", r "10", fill (getPointsColor fillColor) ] []
                ]

        _ ->
            svg [] []


getPointsColor : String -> String
getPointsColor color =
    if color == "black" then
        "white"
    else
        "black"


subscriptions model =
    Sub.none
