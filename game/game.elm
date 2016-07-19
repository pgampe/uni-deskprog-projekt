module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.App exposing (program)
import Markdown
import Element
import Color exposing (..)
import Time exposing (Time, second)
import Mouse exposing (Position)
import Json.Decode exposing (Decoder, (:=))
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)

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


view : Model -> Svg Msg
view { radius, isRed, clicked } =
    Svg.svg [width "100", height "100"]
      [ circle [cx "50", cy "50", r "40", stroke "green", strokeWidth "4", fill "yellow"] []
      , circle [cx "30", cy "30", r (toString radius), stroke "red", strokeWidth "4", fill "blue", onClick Enlarge] []
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
