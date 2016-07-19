module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.App exposing (program)
import Markdown
import Element
import Collage exposing (..)
import Color exposing (..)
import Time exposing (Time, second)
import Mouse exposing (Position)
import Json.Decode exposing (Decoder, (:=))
import Random


type Msg
    = Enlarge
    | Reset
    | Tick Time
    | Shrink
    | Click Position
    | Jump
    | Resize Float


type alias Model =
    { radius : Float, isRed : Bool, clicked : Maybe Position }


init : (Model, Cmd Msg)
init =
    ( { radius = 25, isRed = True, clicked = Nothing }, Cmd.none )


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view { radius, isRed, clicked } =
    div []
        [ h1 [] [ Html.text "Überschrift" ]
        , ol []
            [ li [] [ Html.text "Erstens" ]
            , li [] [ Html.text "Zweitens" ]
            , li [] [ Html.text "Drittens" ]
            ]
        , Markdown.toHtml [] "[link](https://github.com)"
        , Html.text (toString clicked)
        , div [ on "click" (Json.Decode.map Click offsetPosition) ]
            [ Element.toHtml
                <| collage 100
                    100
                    [ filled
                        (if isRed then
                            red
                         else
                            blue
                        )
                        (circle radius)
                    ]
            ]
        , button [ onClick Enlarge ] [ Html.text "Vergrößern" ]
        , button [ onClick Reset ] [ Html.text "Zurücksetzen" ]
        , button [ onClick Jump ] [ Html.text "Zufällig" ]
        ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ radius, isRed } as model) =
    case msg of
        Enlarge ->
            ( { model | radius = radius + 2 }, Cmd.none )

        Shrink ->
            ( { model | radius = radius - 1 }, Cmd.none )

        Reset ->
            init

        Tick _ ->
            ( { model | isRed = not isRed }, Cmd.none )

        Click pos ->
            ( { model | clicked = Just pos }, Cmd.none )

        Jump ->
            ( model, Random.generate Resize (Random.float 0 50) )

        Resize newRadius ->
            ( { model | radius = newRadius }, Cmd.none )


subscriptions { radius } =
    if radius > 20 then
        Sub.batch
            [ Time.every second Tick
            , Mouse.clicks (\_ -> Shrink)
            ]
    else if radius > 0 then
        Mouse.clicks (\_ -> Shrink)
    else
        Sub.none


offsetPosition : Decoder Position
offsetPosition =
    Json.Decode.object2 Position
        ("offsetX" := Json.Decode.int)
        ("offsetY" := Json.Decode.int)
