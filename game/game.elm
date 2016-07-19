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

-- https://de.wikipedia.org/wiki/Datei:Dontworry.svg
view : Model -> Svg Msg
view { radius, isRed, clicked } =
    Svg.svg [width "1500", height "1500"]
      [
       Svg.title [] [Html.text "Don't worry"]
       , desc [] [Html.text "Don't worry"]
       , defs []
          [
          circle [id "cw", cx "0", cy "0", r "50", stroke "black", strokeWidth "7"] []
          , circle [id "cy", cx "0", cy "0", r "50", stroke "black", strokeWidth "7"] []
          , circle [id "cg", cx "0", cy "0", r "50", stroke "black", strokeWidth "7"] []
          , circle [id "cr", cx "0", cy "0", r "50", stroke "black", strokeWidth "7"] []
          , circle [id "cb", cx "0", cy "0", r "50", stroke "black", strokeWidth "7"] []
          , circle [id "csy", cx "0", cy "0", r "40", stroke "black", strokeWidth "7"] []
          , circle [id "csg", cx "0", cy "0", r "40", stroke "black", strokeWidth "7"] []
          , circle [id "csr", cx "0", cy "0", r "40", stroke "black", strokeWidth "7"] []
          , circle [id "csb", cx "0", cy "0", r "40", stroke "black", strokeWidth "7"] []
          , line [id "lh", x1 "0", y1 "0", x2 "25", y2 "0", stroke "black", strokeWidth "7"] []
          , line [id "lv", x1 "0", y1 "0", x2 "0", y2 "25", stroke "black", strokeWidth "7"] []
          , symbol [id "arrow"]
              [
              desc [] [Html.text "arrow"]
              , line [x1 "750", y1 "750", x2 "872", y2 "750", stroke "black", strokeWidth "2"] []
              , line [x1 "750", y1 "750", x2 "742", y2 "742", stroke "black", strokeWidth "2"] []
              , line [x1 "750", y1 "750", x2 "742", y2 "758", stroke "black", strokeWidth "2"] []
              , line [x1 "750", y1 "750", x2 "747", y2 "742", stroke "black", strokeWidth "2"] []
              , line [x1 "755", y1 "750", x2 "747", y2 "758", stroke "black", strokeWidth "2"] []
              , line [x1 "760", y1 "750", x2 "752", y2 "742", stroke "black", strokeWidth "2"] []
              , line [x1 "760", y1 "750", x2 "752", y2 "758", stroke "black", strokeWidth "2"] []
              , line [x1 "765", y1 "750", x2 "757", y2 "758", stroke "black", strokeWidth "2"] []
              , line [x1 "770", y1 "750", x2 "762", y2 "742", stroke "black", strokeWidth "2"] []
              , line [x1 "770", y1 "750", x2 "762", y2 "758", stroke "black", strokeWidth "2"] []
              , Svg.path [d "M 875,750 A 70 20 0 0 1 852,742 l 8,8"] []
              , Svg.path [d "M 875,750 A 70 20 0 0 0 852,758 l 8,-8"] []
              ]
          , symbol [id "ya"]
            [
            desc [] [Html.text "yellow A"]
            , Svg.path [d "M 722,774 l 18,-56 l 20,0 l 18,56 l -8,0 l -6,-16 l -28,0 l -6,16 l -8,0"] []
            , Svg.path [d "M 739,750 l 22,0 l -8,-24 l -6,0 l -8,24", fill "yellow"] []
            ]
          , symbol [id "ra"]
            [
            desc [] [Html.text "red A"]
            , Svg.path [d "M 722,774 l 18,-56 l 20,0 l 18,56 l -8,0 l -6,-16 l -28,0 l -6,16 l -8,0"] []
            , Svg.path [d "M 739,750 l 22,0 l -8,-24 l -6,0 l -8,24", fill "red"] []
            ]
          , symbol [id "ga"]
            [
            desc [] [Html.text "green A"]
            , Svg.path [d "M 722,774 l 18,-56 l 20,0 l 18,56 l -8,0 l -6,-16 l -28,0 l -6,16 l -8,0"] []
            , Svg.path [d "M 739,750 l 22,0 l -8,-24 l -6,0 l -8,24", fill "green"] []
            ]
          , symbol [id "ba"]
            [
            desc [] [Html.text "black A"]
            , Svg.path [d "M 722,774 l 18,-56 l 20,0 l 18,56 l -8,0 l -6,-16 l -28,0 l -6,16 l -8,0", fill "white"] []
            , Svg.path [d "M 739,750 l 22,0 l -8,-24 l -6,0 l -8,24", fill "black"] []
            ]
          , symbol [id "bb"]
            [
            desc [] [Html.text "black B"]
            , Svg.path [d "M 740,770 l 0,-40 l 8,0 A 12,12 0 0 1 750,754 l 0,-8 A 12,12 0 0 1 750,770 l -8,0"] []
            , Svg.path [d "M 747,763 l 0,-9 l 1,0 A 6,4.5 0 0 1 748,763 l -1,0", fill "#ffff80"] []
            , Svg.path [d "M 747,746 l 0,-9 l 1,0 A 6,4.5 0 0 1 748,746 l -1,0", fill "#ffff80"] []
            ]
          ]
       , rect [x "0", y "0", width "1500", height "1500", fill "#ffff80", stroke "red", strokeWidth "35" ] []
       , rect [x "43", y "43", width "1407", height "1407", fill "none", stroke "black", strokeWidth "7" ] []


       , use [x "675", y "125", xlinkHref "#lh"] []
       , use [x "800", y "125", xlinkHref "#lh"] []
       , use [x "625", y "175", xlinkHref "#lv"] []
       , use [x "875", y "175", xlinkHref "#lv"] []
       , use [x "625", y "300", xlinkHref "#lv"] []
       , use [x "875", y "300", xlinkHref "#lv"] []
       , use [x "625", y "425", xlinkHref "#lv"] []
       , use [x "875", y "425", xlinkHref "#lv"] []
       , use [x "625", y "550", xlinkHref "#lv"] []
       , use [x "875", y "550", xlinkHref "#lv"] []
       , use [x "550", y "625", xlinkHref "#lh"] []
       , use [x "925", y "625", xlinkHref "#lh"] []
       , use [x "425", y "625", xlinkHref "#lh"] []
       , use [x "1050", y "625", xlinkHref "#lh"] []
       , use [x "300", y "625", xlinkHref "#lh"] []
       , use [x "1175", y "625", xlinkHref "#lh"] []
       , use [x "175", y "625", xlinkHref "#lh"] []
       , use [x "1300", y "625", xlinkHref "#lh"] []
       , use [x "125", y "675", xlinkHref "#lv"] []
       , use [x "1375", y "675", xlinkHref "#lv"] []
       , use [x "125", y "800", xlinkHref "#lv"] []
       , use [x "1375", y "800", xlinkHref "#lv"] []
       , use [x "175", y "875", xlinkHref "#lh"] []
       , use [x "1300", y "875", xlinkHref "#lh"] []
       , use [x "300", y "875", xlinkHref "#lh"] []
       , use [x "1175", y "875", xlinkHref "#lh"] []
       , use [x "425", y "875", xlinkHref "#lh"] []
       , use [x "1050", y "875", xlinkHref "#lh"] []
       , use [x "550", y "875", xlinkHref "#lh"] []
       , use [x "925", y "875", xlinkHref "#lh"] []
       , use [x "625", y "925", xlinkHref "#lv"] []
       , use [x "875", y "925", xlinkHref "#lv"] []
       , use [x "625", y "1050", xlinkHref "#lv"] []
       , use [x "875", y "1050", xlinkHref "#lv"] []
       , use [x "625", y "1175", xlinkHref "#lv"] []
       , use [x "875", y "1175", xlinkHref "#lv"] []
       , use [x "625", y "1300", xlinkHref "#lv"] []
       , use [x "875", y "1300", xlinkHref "#lv"] []
       , use [x "675", y "1375", xlinkHref "#lh"] []
       , use [x "800", y "1375", xlinkHref "#lh"] []


       , use [x "875", y "125", xlinkHref "#cg", fill "green"] []
       , use [x "875", y "250", xlinkHref "#cw", fill "white"] []
       , use [x "875", y "375", xlinkHref "#cw", fill "white"] []
       , use [x "875", y "500", xlinkHref "#cw", fill "white"] []
       , use [x "875", y "625", xlinkHref "#cw", fill "white"] []

       , use [x "1000", y "625", xlinkHref "#cw", fill "white"] []
       , use [x "1125", y "625", xlinkHref "#cw", fill "white"] []
       , use [x "1250", y "625", xlinkHref "#cw", fill "white"] []
       , use [x "1375", y "625", xlinkHref "#cw", fill "white"] []

       , use [x "1375", y "750", xlinkHref "#cw", fill "white"] []
       , use [x "1375", y "875", xlinkHref "#cr", fill "red"] []

       , use [x "875", y "875", xlinkHref "#cw", fill "white"] []
       , use [x "1250", y "875", xlinkHref "#cw", fill "white"] []
       , use [x "1125", y "875", xlinkHref "#cw", fill "white"] []
       , use [x "1000", y "875", xlinkHref "#cw", fill "white"] []

       , use [x "875", y "1000", xlinkHref "#cw", fill "white"] []
       , use [x "875", y "1125", xlinkHref "#cw", fill "white"] []
       , use [x "875", y "1250", xlinkHref "#cw", fill "white"] []
       , use [x "875", y "1375", xlinkHref "#cw", fill "white"] []


       , use [x "750", y "1375", xlinkHref "#cw", fill "white"] []
       , use [x "625", y "1375", xlinkHref "#cb", fill "black"] []

       , use [x "625", y "1250", xlinkHref "#cw", fill "white"] []
       , use [x "625", y "1125", xlinkHref "#cw", fill "white"] []
       , use [x "625", y "1000", xlinkHref "#cw", fill "white"] []
       , use [x "625", y "875", xlinkHref "#cw", fill "white"] []

       , use [x "500", y "875", xlinkHref "#cw", fill "white"] []
       , use [x "375", y "875", xlinkHref "#cw", fill "white"] []
       , use [x "250", y "875", xlinkHref "#cw", fill "white"] []
       , use [x "125", y "875", xlinkHref "#cw", fill "white"] []

       , use [x "125", y "750", xlinkHref "#cw", fill "white"] []
       , use [x "125", y "625", xlinkHref "#cy", fill "yellow"] []

       , use [x "250", y "625", xlinkHref "#cw", fill "white"] []
       , use [x "375", y "625", xlinkHref "#cw", fill "white"] []
       , use [x "500", y "625", xlinkHref "#cw", fill "white"] []
       , use [x "625", y "625", xlinkHref "#cw", fill "white"] []

       , use [x "625", y "500", xlinkHref "#cw", fill "white"] []
       , use [x "625", y "375", xlinkHref "#cw", fill "white"] []
       , use [x "625", y "250", xlinkHref "#cw", fill "white"] []
       , use [x "625", y "125", xlinkHref "#cw", fill "white"] []

       , use [x "750", y "125", xlinkHref "#cw", fill "white"] []



       , use [x "750", y "250", xlinkHref "#csg", fill "green"] []
       , use [x "750", y "375", xlinkHref "#csg", fill "green"] []
       , use [x "750", y "500", xlinkHref "#csg", fill "green"] []
       , use [x "750", y "625", xlinkHref "#csg", fill "green"] []

       , use [x "750", y "875", xlinkHref "#csb", fill "black"] []
       , use [x "750", y "1000", xlinkHref "#csb", fill "black"] []
       , use [x "750", y "1125", xlinkHref "#csb", fill "black"] []
       , use [x "750", y "1250", xlinkHref "#csb", fill "black"] []

       , use [x "250", y "750", xlinkHref "#csy", fill "yellow"] []
       , use [x "375", y "750", xlinkHref "#csy", fill "yellow"] []
       , use [x "500", y "750", xlinkHref "#csy", fill "yellow"] []
       , use [x "625", y "750", xlinkHref "#csy", fill "yellow"] []

       , use [x "875", y "750", xlinkHref "#csr", fill "red"] []
       , use [x "1000", y "750", xlinkHref "#csr", fill "red"] []
       , use [x "1125", y "750", xlinkHref "#csr", fill "red"] []
       , use [x "1250", y "750", xlinkHref "#csr", fill "red"] []



       , use [x "1250", y "125", xlinkHref "#csg", fill "green"] []
       , use [x "1375", y "125", xlinkHref "#csg", fill "green"] []
       , use [x "1250", y "250", xlinkHref "#csg", fill "green"] []
       , use [x "1375", y "250", xlinkHref "#csg", fill "green"] []

       , use [x "125", y "1250", xlinkHref "#csb", fill "black"] []
       , use [x "250", y "1250", xlinkHref "#csb", fill "black"] []
       , use [x "125", y "1375", xlinkHref "#csb", fill "black"] []
       , use [x "250", y "1375", xlinkHref "#csb", fill "black"] []

       , use [x "125", y "125", xlinkHref "#csy", fill "yellow"] []
       , use [x "250", y "125", xlinkHref "#csy", fill "yellow"] []
       , use [x "125", y "250", xlinkHref "#csy", fill "yellow"] []
       , use [x "250", y "250", xlinkHref "#csy", fill "yellow"] []

       , use [x "1250", y "1250", xlinkHref "#csr", fill "red"] []
       , use [x "1375", y "1250", xlinkHref "#csr", fill "red"] []
       , use [x "1250", y "1375", xlinkHref "#csr", fill "red"] []
       , use [x "1375", y "1375", xlinkHref "#csr", fill "red"] []


       , use [xlinkHref "#arrow", transform "translate(-650,-240)"] []
       , use [xlinkHref "#arrow", transform "translate(650,240) rotate(180,750,750)"] []
       , use [xlinkHref "#arrow", transform "translate(240,-650) rotate(90,750,750)"] []
       , use [xlinkHref "#arrow", transform "translate(-240,650) rotate(270,750,750)"] []

       , use [xlinkHref "#ya", transform "translate(-625,-125)"] []
       , use [xlinkHref "#ga", transform "translate(125,-625)"] []
       , use [xlinkHref "#ra", transform "translate(625,125)"] []
       , use [xlinkHref "#ba", transform "translate(-125,625)"] []

       , use [xlinkHref "#bb", transform "translate(-563,563)"] []
       , use [xlinkHref "#bb", transform "translate(563,563)"] []
       , use [xlinkHref "#bb", transform "translate(-563,-563)"] []
       , use [xlinkHref "#bb", transform "translate(563,-563)"] []
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
