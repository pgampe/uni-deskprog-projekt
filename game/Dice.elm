module Dice exposing (..)

import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Random


type alias Model =
    Int


init : Model
init =
    1


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
    viewRender [ onClick Roll ] model


viewRender attributes model =
    case model of
        1 ->
            svg attributes
                [ rect [ rx "4", cy "4", width "100", height "100", rx "7", fill "white", stroke "black", strokeWidth "8" ] []
                , circle [ cx "50", cy "50", r "10", fill "black" ] []
                ]

        2 ->
            svg attributes
                [ rect [ rx "4", cy "4", width "100", height "100", rx "7", fill "white", stroke "black", strokeWidth "8" ] []
                , circle [ cx "30", cy "50", r "10", fill "black" ] []
                , circle [ cx "70", cy "50", r "10", fill "black" ] []
                ]

        3 ->
            svg attributes
                [ rect [ rx "4", cy "4", width "100", height "100", rx "7", fill "white", stroke "black", strokeWidth "8" ] []
                , circle [ cx "25", cy "50", r "10", fill "black" ] []
                , circle [ cx "50", cy "50", r "10", fill "black" ] []
                , circle [ cx "75", cy "50", r "10", fill "black" ] []
                ]

        4 ->
            svg attributes
                [ rect [ rx "4", cy "4", width "100", height "100", rx "7", fill "white", stroke "black", strokeWidth "8" ] []
                , circle [ cx "30", cy "30", r "10", fill "black" ] []
                , circle [ cx "70", cy "30", r "10", fill "black" ] []
                , circle [ cx "30", cy "70", r "10", fill "black" ] []
                , circle [ cx "70", cy "70", r "10", fill "black" ] []
                ]

        5 ->
            svg attributes
                [ rect [ rx "4", cy "4", width "100", height "100", rx "7", fill "white", stroke "black", strokeWidth "8" ] []
                , circle [ cx "30", cy "30", r "10", fill "black" ] []
                , circle [ cx "70", cy "30", r "10", fill "black" ] []
                , circle [ cx "50", cy "50", r "10", fill "black" ] []
                , circle [ cx "30", cy "70", r "10", fill "black" ] []
                , circle [ cx "70", cy "70", r "10", fill "black" ] []
                ]

        6 ->
            svg attributes
                [ rect [ rx "4", cy "4", width "100", height "100", rx "7", fill "white", stroke "black", strokeWidth "8" ] []
                , circle [ cx "25", cy "30", r "10", fill "black" ] []
                , circle [ cx "50", cy "30", r "10", fill "black" ] []
                , circle [ cx "75", cy "30", r "10", fill "black" ] []
                , circle [ cx "25", cy "70", r "10", fill "black" ] []
                , circle [ cx "50", cy "70", r "10", fill "black" ] []
                , circle [ cx "75", cy "70", r "10", fill "black" ] []
                ]

        _ ->
            svg [] []


subscriptions model =
    Sub.none
