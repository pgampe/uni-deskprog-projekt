module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Html.App as App
import Random
import List exposing (..)
import Dice
import Debug exposing (log)
import Array exposing (..)


type alias Piece =
    { id : Int, active : Bool, position : PiecePosition, relativePosition : Int }


type alias Player =
    { offset : Int, pieces : List Piece, pColor : String }


type alias PiecePosition =
    { id : Int, x : Int, y : Int, fill : String, xlinkHref : String }


type alias Model =
    { players : Array Player
    , currentPlayer : Int
    , dice : Dice.Model
    , countOfRolls : Int
    , playerNeedsToMakeMove : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { players = getPlayersWithInitialPositions
      , currentPlayer = 0
      , dice = (fst Dice.init)
      , countOfRolls = 0
      , playerNeedsToMakeMove = False
      }
    , Cmd.none
    )


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Reset
    | Jump
    | RollDice
    | SetDice Int
    | MovePiece Piece



{- updateTags state someNewTags =
   let value = state.currentUserValues
   in { state | currentUserValues <- { value | tags <- someNewTags } }

   für state.currentUserValues.tags = [tags]
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        Jump ->
            ( model, Cmd.none )

        RollDice ->
            ( model, Random.generate SetDice (Random.int 1 6) )

        SetDice newDiceValue ->
            ( { model
                | dice = newDiceValue
                , countOfRolls = (updateRollsCountAfterDiceRoll model.countOfRolls newDiceValue)
                , playerNeedsToMakeMove = (doesPlayerNeedToMoveAfterRoll model newDiceValue)
                , players = (updatePlayersAfterRoll model newDiceValue)
                , currentPlayer = (updateCurrentPlayerAfterDiceRoll model newDiceValue)
              }
            , Cmd.none
            )

        MovePiece piece ->
            ( { model
                | playerNeedsToMakeMove = False
                , countOfRolls = 0
                , players = (updatePlayersAfterMove model piece)
                , currentPlayer = (updateCurrentPlayerAfterMove model)
              }
            , Cmd.none
            )


subscriptions model =
    Sub.none



-- https://de.wikipedia.org/wiki/Datei:Dontworry.svg


view model =
    Svg.svg [ width "1500", height "1700" ]
        (concat
            (renderBoardList model)
            +++ (getSvgForDice model)
        )


(+++) : List a -> List a -> List a
(+++) =
    List.append
infixr 5 +++


updateCurrentPlayerAfterDiceRoll : Model -> Dice.Model -> Int
updateCurrentPlayerAfterDiceRoll model currentDiceValue =
    if currentDiceValue == 6 || log "value of dice roll count in model before current player update" (model.countOfRolls) < 2 then
        model.currentPlayer
    else
        (model.currentPlayer + 1) % 4


updateCurrentPlayerAfterMove : Model -> Int
updateCurrentPlayerAfterMove model =
    if model.dice == 6 then
        model.currentPlayer
    else
        (model.currentPlayer + 1) % 4


updatePlayersAfterMove : Model -> Piece -> Array Player
updatePlayersAfterMove model piece =
    Array.set model.currentPlayer (updatePlayerAfterMove model piece) model.players


updatePlayerAfterMove : Model -> Piece -> Player
updatePlayerAfterMove model piece =
    let
        player =
            getCurrentPlayer model

        position =
            piece.position

        currentPosition =
            position.id - player.offset
    in
        if (log "current position" currentPosition) > 100 then
            -- opening
            { player | pieces = (List.map (\currentPiece -> updatePiecePositionInPositions currentPiece piece.id (1 + player.offset)) player.pieces) }
        else
            case (currentPosition) of
                39 ->
                    -- todo: actually check the position (empty, etc)
                    if (model.dice < 6) then
                        { player | pieces = (List.map (\currentPiece -> updatePiecePositionInPositions currentPiece piece.id (calculateNewPosition currentPosition model.dice player.offset)) player.pieces) }
                    else
                        player

                40 ->
                    if (model.dice < 5) then
                        { player | pieces = (List.map (\currentPiece -> updatePiecePositionInPositions currentPiece piece.id (calculateNewPosition currentPosition model.dice player.offset)) player.pieces) }
                    else
                        player

                41 ->
                    if (model.dice < 4) then
                        { player | pieces = (List.map (\currentPiece -> updatePiecePositionInPositions currentPiece piece.id (calculateNewPosition currentPosition model.dice player.offset)) player.pieces) }
                    else
                        player

                42 ->
                    if (model.dice < 3) then
                        { player | pieces = (List.map (\currentPiece -> updatePiecePositionInPositions currentPiece piece.id (calculateNewPosition currentPosition model.dice player.offset)) player.pieces) }
                    else
                        player

                43 ->
                    if (model.dice < 2) then
                        { player | pieces = (List.map (\currentPiece -> updatePiecePositionInPositions currentPiece piece.id (calculateNewPosition currentPosition model.dice player.offset)) player.pieces) }
                    else
                        player

                -- no further move possible
                44 ->
                    player

                -- todo: check of field is empty before moving
                _ ->
                    { player | pieces = (List.map (\currentPiece -> updatePiecePositionInPositions currentPiece piece.id (log "next position" (calculateNewPosition currentPosition model.dice player.offset))) player.pieces) }


calculateNewPosition : Int -> Dice.Model -> Int -> Int
calculateNewPosition currentPosition currentDiceValue playerOffset =
    let
        newPosition =
            currentPosition + currentDiceValue

        realPosition =
            newPosition + playerOffset
    in
        if newPosition < 41 then
            if (realPosition % 40) == 0 then
                40
            else
                realPosition % 40
        else
            realPosition


updatePiecePositionInPositions : Piece -> Int -> Int -> Piece
updatePiecePositionInPositions piece pieceId positionId =
    if piece.id == pieceId then
        { piece | position = getPositionFromPositions positionId }
    else
        { piece
            | active =
                if piece.position.id < 100 then
                    True
                else
                    False
        }


getFirstUnsafe : List a -> a
getFirstUnsafe list =
    case list of
        x :: xs ->
            x

        [] ->
            -- https://github.com/elm-lang/core/issues/215
            Debug.crash "List cannot be empty"


doesPlayerNeedToMoveAfterRoll : Model -> Dice.Model -> Bool
doesPlayerNeedToMoveAfterRoll model currentDiceValue =
    playerHasPiecesInGame model || currentDiceValue == 6


updatePlayersAfterRoll : Model -> Dice.Model -> Array Player
updatePlayersAfterRoll model diceValue =
    Array.set model.currentPlayer (updatePlayerAfterRoll (getCurrentPlayer model) diceValue) model.players


updatePlayerAfterRoll : Player -> Dice.Model -> Player
updatePlayerAfterRoll player diceValue =
    case diceValue of
        6 ->
            { player | pieces = makeNextPieceActive player.pieces }

        _ ->
            player


makeNextPieceActive : List Piece -> List Piece
makeNextPieceActive pieces =
    let
        currentPiece =
            getFirstUnsafe pieces

        rest =
            drop 1 pieces
    in
        if currentPiece.active then
            currentPiece :: makeNextPieceActive rest
        else
            togglePieceActive (currentPiece) :: rest


activePiecesInList : List Piece -> Bool
activePiecesInList pieces =
    (List.foldl (\pc c -> pc.active || c) False pieces)


togglePieceActive : Piece -> Piece
togglePieceActive piece =
    if piece.active then
        { piece | active = False }
    else
        { piece | active = True }


updateRollsCountAfterDiceRoll : Int -> Dice.Model -> Int
updateRollsCountAfterDiceRoll current new =
    if new == 6 then
        0
    else
        case current of
            3 ->
                Debug.crash "Why did we hit three rolls in a row?"

            2 ->
                0

            1 ->
                2

            0 ->
                1

            _ ->
                Debug.crash "We should not roll more than three times"


shouldRoleDice : Model -> Bool
shouldRoleDice model =
    (model.countOfRolls < 3 || (model.dice == 6 && playerHasPiecesInGame model)) && not model.playerNeedsToMakeMove


getCurrentPlayer : Model -> Player
getCurrentPlayer model =
    case (Array.get model.currentPlayer model.players) of
        Nothing ->
            Debug.crash "Wrong player selected"

        Just player ->
            player


playerHasPiecesInGame : Model -> Bool
playerHasPiecesInGame model =
    let
        player =
            getCurrentPlayer model
    in
        activePiecesInList player.pieces


getSvgForDice model =
    let
        message =
            if shouldRoleDice model then
                [ onClick RollDice ]
            else
                []
    in
        [ (Dice.viewRender (message +++ [ x "100", y "1550" ]) model.dice (getDiceColor model)) ]


getDiceColor : Model -> String
getDiceColor model =
    let
        player =
            getCurrentPlayer model
    in
        if model.playerNeedsToMakeMove then
            "white"
        else
            player.pColor


renderBoardList model =
    [ svgbasics
    , svgdefs
    , svgoutersquares
    , svglines
    , svgarrows
    , svgletters
    , (positionsToSvg availablePositions)
    , (svgPlayerPositions model)
    ]


getPositionFromPositions : Int -> PiecePosition
getPositionFromPositions x =
    case head (List.filter (\pos -> x == pos.id) availablePositions) of
        Nothing ->
            { id = 9999, x = 0, y = 0, xlinkHref = "#cg", fill = "blue" }

        Just x ->
            x


getPlayersWithInitialPositions : Array Player
getPlayersWithInitialPositions =
    Array.fromList
        [ { offset = 0
          , pColor = "green"
          , pieces =
                [ { id = 1, active = False, position = getPositionFromPositions 101, relativePosition = 0 }
                , { id = 2, active = False, position = getPositionFromPositions 102, relativePosition = 0 }
                , { id = 3, active = False, position = getPositionFromPositions 103, relativePosition = 0 }
                , { id = 4, active = False, position = getPositionFromPositions 104, relativePosition = 0 }
                ]
          }
        , { offset = 10
          , pColor = "red"
          , pieces =
                [ { id = 11, active = False, position = getPositionFromPositions 111, relativePosition = 0 }
                , { id = 12, active = False, position = getPositionFromPositions 112, relativePosition = 0 }
                , { id = 13, active = False, position = getPositionFromPositions 113, relativePosition = 0 }
                , { id = 14, active = False, position = getPositionFromPositions 114, relativePosition = 0 }
                ]
          }
        , { offset = 20
          , pColor = "black"
          , pieces =
                [ { id = 21, active = False, position = getPositionFromPositions 121, relativePosition = 0 }
                , { id = 22, active = False, position = getPositionFromPositions 122, relativePosition = 0 }
                , { id = 23, active = False, position = getPositionFromPositions 123, relativePosition = 0 }
                , { id = 24, active = False, position = getPositionFromPositions 124, relativePosition = 0 }
                ]
          }
        , { offset = 30
          , pColor = "yellow"
          , pieces =
                [ { id = 31, active = False, position = getPositionFromPositions 131, relativePosition = 0 }
                , { id = 32, active = False, position = getPositionFromPositions 132, relativePosition = 0 }
                , { id = 33, active = False, position = getPositionFromPositions 133, relativePosition = 0 }
                , { id = 34, active = False, position = getPositionFromPositions 134, relativePosition = 0 }
                ]
          }
        ]


svgPlayerPositions model =
    concatMap playerToPiecesAndColor (Array.toList model.players) |> List.map (\( piece, color ) -> svgFromPieceAndColor piece color model.playerNeedsToMakeMove (getCurrentPlayer model))


playerToPiecesAndColor : Player -> List ( Piece, String )
playerToPiecesAndColor player =
    List.map (\piece -> ( piece, player.pColor )) player.pieces


svgFromPieceAndColor : Piece -> String -> Bool -> Player -> Svg Msg
svgFromPieceAndColor piece color piecesCanMove currentPlayer =
    let
        position =
            piece.position
    in
        if piece.active && piecesCanMove && (currentPlayer.pColor == color) then
            use [ id (toString piece.position.id), x (toString (position.x - 50)), y (toString (position.y - 130)), xlinkHref "#piece", fill color, onClick (MovePiece piece) ] []
        else
            use [ id (toString piece.position.id), x (toString (position.x - 50)), y (toString (position.y - 130)), xlinkHref "#piece", fill color ] []


svgbasics =
    [ Svg.title [] [ Svg.text "Don't worry" ]
    , desc [] [ Svg.text "Don't worry" ]
    ]


svgdefs : List (Svg defs)
svgdefs =
    [ defs []
        [ circle [ id "cw", cx "0", cy "0", r "50", stroke "black", strokeWidth "7" ] []
        , circle [ id "cy", cx "0", cy "0", r "50", stroke "black", strokeWidth "7" ] []
        , circle [ id "cg", cx "0", cy "0", r "50", stroke "black", strokeWidth "7" ] []
        , circle [ id "cr", cx "0", cy "0", r "50", stroke "black", strokeWidth "7" ] []
        , circle [ id "cb", cx "0", cy "0", r "50", stroke "black", strokeWidth "7" ] []
        , circle [ id "csy", cx "0", cy "0", r "40", stroke "black", strokeWidth "7" ] []
        , circle [ id "csg", cx "0", cy "0", r "40", stroke "black", strokeWidth "7" ] []
        , circle [ id "csr", cx "0", cy "0", r "40", stroke "black", strokeWidth "7" ] []
        , circle [ id "csb", cx "0", cy "0", r "40", stroke "black", strokeWidth "7" ] []
        , line [ id "lh", x1 "0", y1 "0", x2 "25", y2 "0", stroke "black", strokeWidth "7" ] []
        , line [ id "lv", x1 "0", y1 "0", x2 "0", y2 "25", stroke "black", strokeWidth "7" ] []
        , symbol [ id "arrow" ]
            [ desc [] [ Svg.text "arrow" ]
            , line [ x1 "750", y1 "750", x2 "872", y2 "750", stroke "black", strokeWidth "2" ] []
            , line [ x1 "750", y1 "750", x2 "742", y2 "742", stroke "black", strokeWidth "2" ] []
            , line [ x1 "750", y1 "750", x2 "742", y2 "758", stroke "black", strokeWidth "2" ] []
            , line [ x1 "755", y1 "750", x2 "747", y2 "742", stroke "black", strokeWidth "2" ] []
            , line [ x1 "755", y1 "750", x2 "747", y2 "758", stroke "black", strokeWidth "2" ] []
            , line [ x1 "760", y1 "750", x2 "752", y2 "742", stroke "black", strokeWidth "2" ] []
            , line [ x1 "760", y1 "750", x2 "752", y2 "758", stroke "black", strokeWidth "2" ] []
            , line [ x1 "765", y1 "750", x2 "757", y2 "742", stroke "black", strokeWidth "2" ] []
            , line [ x1 "765", y1 "750", x2 "757", y2 "758", stroke "black", strokeWidth "2" ] []
            , line [ x1 "770", y1 "750", x2 "762", y2 "742", stroke "black", strokeWidth "2" ] []
            , line [ x1 "770", y1 "750", x2 "762", y2 "758", stroke "black", strokeWidth "2" ] []
            , Svg.path [ d "M 875,750 A 70 20 0 0 1 852,742 l 8,8" ] []
            , Svg.path [ d "M 875,750 A 70 20 0 0 0 852,758 l 8,-8" ] []
            ]
        , symbol [ id "ya" ]
            [ desc [] [ Svg.text "yellow A" ]
            , Svg.path [ d "M 722,774 l 18,-56 l 20,0 l 18,56 l -8,0 l -6,-16 l -28,0 l -6,16 l -8,0" ] []
            , Svg.path [ d "M 739,750 l 22,0 l -8,-24 l -6,0 l -8,24", fill "yellow" ] []
            ]
        , symbol [ id "ra" ]
            [ desc [] [ Svg.text "red A" ]
            , Svg.path [ d "M 722,774 l 18,-56 l 20,0 l 18,56 l -8,0 l -6,-16 l -28,0 l -6,16 l -8,0" ] []
            , Svg.path [ d "M 739,750 l 22,0 l -8,-24 l -6,0 l -8,24", fill "red" ] []
            ]
        , symbol [ id "ga" ]
            [ desc [] [ Svg.text "green A" ]
            , Svg.path [ d "M 722,774 l 18,-56 l 20,0 l 18,56 l -8,0 l -6,-16 l -28,0 l -6,16 l -8,0" ] []
            , Svg.path [ d "M 739,750 l 22,0 l -8,-24 l -6,0 l -8,24", fill "green" ] []
            ]
        , symbol [ id "ba" ]
            [ desc [] [ Svg.text "black A" ]
            , Svg.path [ d "M 722,774 l 18,-56 l 20,0 l 18,56 l -8,0 l -6,-16 l -28,0 l -6,16 l -8,0", fill "white" ] []
            , Svg.path [ d "M 739,750 l 22,0 l -8,-24 l -6,0 l -8,24", fill "black" ] []
            ]
        , symbol [ id "bb" ]
            [ desc [] [ Svg.text "black B" ]
            , Svg.path [ d "M 740,770 l 0,-40 l 8,0 A 12,12 0 0 1 750,754 l 0,-8 A 12,12 0 0 1 750,770 l -8,0" ] []
            , Svg.path [ d "M 747,763 l 0,-9 l 1,0 A 6,4.5 0 0 1 748,763 l -1,0", fill "#ffff80" ] []
            , Svg.path [ d "M 747,746 l 0,-9 l 1,0 A 6,4.5 0 0 1 748,746 l -1,0", fill "#ffff80" ] []
            ]
        , linearGradient [ id "GradientPiece" ]
            [ stop [ offset "0", stopColor "white", stopOpacity "0" ] []
            , stop [ offset "1", stopColor "white", stopOpacity "1" ] []
            ]
        , Svg.mask [ id "MaskPiece" ]
            [ polygon [ points "10,0 90,0 90,180 10,180", fill "url(#GradientPiece)" ] []
            ]
        , svg [ id "piece" ]
            [ circle [ cx "50", cy "40", r "30" ] []
            , circle [ cx "50", cy "40", r "30", fill "black", Svg.Attributes.mask "url(#MaskPiece)" ] []
            , Svg.path [ d "M50,30 L90,150 Q 50,180 10,150" ] []
            , Svg.path [ d "M50,30 L90,150 Q 50,180 10,150", fill "black", Svg.Attributes.mask "url(#MaskPiece)" ] []
            ]
        ]
    ]


svgoutersquares =
    [ rect [ x "0", y "0", width "1500", height "1700", fill "#ffff80", stroke "red", strokeWidth "35" ] []
    , rect [ x "43", y "43", width "1407", height "1407", fill "none", stroke "black", strokeWidth "7" ] []
    ]


svglines : List (Svg use)
svglines =
    [ use [ x "675", y "125", xlinkHref "#lh" ] []
    , use [ x "800", y "125", xlinkHref "#lh" ] []
    , use [ x "625", y "175", xlinkHref "#lv" ] []
    , use [ x "875", y "175", xlinkHref "#lv" ] []
    , use [ x "625", y "300", xlinkHref "#lv" ] []
    , use [ x "875", y "300", xlinkHref "#lv" ] []
    , use [ x "625", y "425", xlinkHref "#lv" ] []
    , use [ x "875", y "425", xlinkHref "#lv" ] []
    , use [ x "625", y "550", xlinkHref "#lv" ] []
    , use [ x "875", y "550", xlinkHref "#lv" ] []
    , use [ x "550", y "625", xlinkHref "#lh" ] []
    , use [ x "925", y "625", xlinkHref "#lh" ] []
    , use [ x "425", y "625", xlinkHref "#lh" ] []
    , use [ x "1050", y "625", xlinkHref "#lh" ] []
    , use [ x "300", y "625", xlinkHref "#lh" ] []
    , use [ x "1175", y "625", xlinkHref "#lh" ] []
    , use [ x "175", y "625", xlinkHref "#lh" ] []
    , use [ x "1300", y "625", xlinkHref "#lh" ] []
    , use [ x "125", y "675", xlinkHref "#lv" ] []
    , use [ x "1375", y "675", xlinkHref "#lv" ] []
    , use [ x "125", y "800", xlinkHref "#lv" ] []
    , use [ x "1375", y "800", xlinkHref "#lv" ] []
    , use [ x "175", y "875", xlinkHref "#lh" ] []
    , use [ x "1300", y "875", xlinkHref "#lh" ] []
    , use [ x "300", y "875", xlinkHref "#lh" ] []
    , use [ x "1175", y "875", xlinkHref "#lh" ] []
    , use [ x "425", y "875", xlinkHref "#lh" ] []
    , use [ x "1050", y "875", xlinkHref "#lh" ] []
    , use [ x "550", y "875", xlinkHref "#lh" ] []
    , use [ x "925", y "875", xlinkHref "#lh" ] []
    , use [ x "625", y "925", xlinkHref "#lv" ] []
    , use [ x "875", y "925", xlinkHref "#lv" ] []
    , use [ x "625", y "1050", xlinkHref "#lv" ] []
    , use [ x "875", y "1050", xlinkHref "#lv" ] []
    , use [ x "625", y "1175", xlinkHref "#lv" ] []
    , use [ x "875", y "1175", xlinkHref "#lv" ] []
    , use [ x "625", y "1300", xlinkHref "#lv" ] []
    , use [ x "875", y "1300", xlinkHref "#lv" ] []
    , use [ x "675", y "1375", xlinkHref "#lh" ] []
    , use [ x "800", y "1375", xlinkHref "#lh" ] []
    ]


svgarrows : List (Svg use)
svgarrows =
    [ use [ xlinkHref "#arrow", transform "translate(-650,-240)" ] []
    , use [ xlinkHref "#arrow", transform "translate(650,240) rotate(180,750,750)" ] []
    , use [ xlinkHref "#arrow", transform "translate(240,-650) rotate(90,750,750)" ] []
    , use [ xlinkHref "#arrow", transform "translate(-240,650) rotate(270,750,750)" ] []
    ]


svgletters : List (Svg use)
svgletters =
    [ use [ xlinkHref "#ya", transform "translate(-625,-125)" ] []
    , use [ xlinkHref "#ga", transform "translate(125,-625)" ] []
    , use [ xlinkHref "#ra", transform "translate(625,125)" ] []
    , use [ xlinkHref "#ba", transform "translate(-125,625)" ] []
    , use [ xlinkHref "#bb", transform "translate(-563,563)" ] []
    , use [ xlinkHref "#bb", transform "translate(563,563)" ] []
    , use [ xlinkHref "#bb", transform "translate(-563,-563)" ] []
    , use [ xlinkHref "#bb", transform "translate(563,-563)" ] []
    ]


positionsToSvg : List PiecePosition -> List (Svg use)
positionsToSvg piecepositions =
    List.map positionToSvg piecepositions


positionToSvg : PiecePosition -> Svg use
positionToSvg p =
    use [ x (toString p.x), y (toString p.y), xlinkHref p.xlinkHref, fill p.fill, id (toString p.id) ] []


availablePositions : List PiecePosition
availablePositions =
    [ { id = 1, x = 875, y = 125, xlinkHref = "#cg", fill = "green" }
    , { id = 2, x = 875, y = 250, xlinkHref = "#cw", fill = "white" }
    , { id = 3, x = 875, y = 375, xlinkHref = "#cw", fill = "white" }
    , { id = 4, x = 875, y = 500, xlinkHref = "#cw", fill = "white" }
    , { id = 5, x = 875, y = 625, xlinkHref = "#cw", fill = "white" }
    , { id = 6, x = 1000, y = 625, xlinkHref = "#cw", fill = "white" }
    , { id = 7, x = 1125, y = 625, xlinkHref = "#cw", fill = "white" }
    , { id = 8, x = 1250, y = 625, xlinkHref = "#cw", fill = "white" }
    , { id = 9, x = 1375, y = 625, xlinkHref = "#cw", fill = "white" }
    , { id = 10, x = 1375, y = 750, xlinkHref = "#cw", fill = "white" }
    , { id = 11, x = 1375, y = 875, xlinkHref = "#cr", fill = "red" }
    , { id = 12, x = 1250, y = 875, xlinkHref = "#cw", fill = "white" }
    , { id = 13, x = 1125, y = 875, xlinkHref = "#cw", fill = "white" }
    , { id = 14, x = 1000, y = 875, xlinkHref = "#cw", fill = "white" }
    , { id = 15, x = 875, y = 875, xlinkHref = "#cw", fill = "white" }
    , { id = 16, x = 875, y = 1000, xlinkHref = "#cw", fill = "white" }
    , { id = 17, x = 875, y = 1125, xlinkHref = "#cw", fill = "white" }
    , { id = 18, x = 875, y = 1250, xlinkHref = "#cw", fill = "white" }
    , { id = 19, x = 875, y = 1375, xlinkHref = "#cw", fill = "white" }
    , { id = 20, x = 750, y = 1375, xlinkHref = "#cw", fill = "white" }
    , { id = 21, x = 625, y = 1375, xlinkHref = "#cb", fill = "black" }
    , { id = 22, x = 625, y = 1250, xlinkHref = "#cw", fill = "white" }
    , { id = 23, x = 625, y = 1125, xlinkHref = "#cw", fill = "white" }
    , { id = 24, x = 625, y = 1000, xlinkHref = "#cw", fill = "white" }
    , { id = 25, x = 625, y = 875, xlinkHref = "#cw", fill = "white" }
    , { id = 26, x = 500, y = 875, xlinkHref = "#cw", fill = "white" }
    , { id = 27, x = 375, y = 875, xlinkHref = "#cw", fill = "white" }
    , { id = 28, x = 250, y = 875, xlinkHref = "#cw", fill = "white" }
    , { id = 29, x = 125, y = 875, xlinkHref = "#cw", fill = "white" }
    , { id = 30, x = 125, y = 750, xlinkHref = "#cw", fill = "white" }
    , { id = 31, x = 125, y = 625, xlinkHref = "#cy", fill = "yellow" }
    , { id = 32, x = 250, y = 625, xlinkHref = "#cw", fill = "white" }
    , { id = 33, x = 375, y = 625, xlinkHref = "#cw", fill = "white" }
    , { id = 34, x = 500, y = 625, xlinkHref = "#cw", fill = "white" }
    , { id = 35, x = 625, y = 625, xlinkHref = "#cw", fill = "white" }
    , { id = 36, x = 625, y = 500, xlinkHref = "#cw", fill = "white" }
    , { id = 37, x = 625, y = 375, xlinkHref = "#cw", fill = "white" }
    , { id = 38, x = 625, y = 250, xlinkHref = "#cw", fill = "white" }
    , { id = 39, x = 625, y = 125, xlinkHref = "#cw", fill = "white" }
    , { id = 40, x = 750, y = 125, xlinkHref = "#cw", fill = "white" }
    , { id = 41, x = 750, y = 250, xlinkHref = "#csg", fill = "green" }
    , { id = 42, x = 750, y = 375, xlinkHref = "#csg", fill = "green" }
    , { id = 43, x = 750, y = 500, xlinkHref = "#csg", fill = "green" }
    , { id = 44, x = 750, y = 625, xlinkHref = "#csg", fill = "green" }
    , { id = 51, x = 875, y = 750, xlinkHref = "#csr", fill = "red" }
    , { id = 52, x = 1000, y = 750, xlinkHref = "#csr", fill = "red" }
    , { id = 53, x = 1125, y = 750, xlinkHref = "#csr", fill = "red" }
    , { id = 54, x = 1250, y = 750, xlinkHref = "#csr", fill = "red" }
    , { id = 61, x = 750, y = 875, xlinkHref = "#csb", fill = "black" }
    , { id = 62, x = 750, y = 1000, xlinkHref = "#csb", fill = "black" }
    , { id = 63, x = 750, y = 1125, xlinkHref = "#csb", fill = "black" }
    , { id = 64, x = 750, y = 1250, xlinkHref = "#csb", fill = "black" }
    , { id = 71, x = 250, y = 750, xlinkHref = "#csy", fill = "yellow" }
    , { id = 72, x = 375, y = 750, xlinkHref = "#csy", fill = "yellow" }
    , { id = 73, x = 500, y = 750, xlinkHref = "#csy", fill = "yellow" }
    , { id = 74, x = 625, y = 750, xlinkHref = "#csy", fill = "yellow" }
    , { id = 101, x = 1250, y = 125, xlinkHref = "#csg", fill = "green" }
    , { id = 102, x = 1375, y = 125, xlinkHref = "#csg", fill = "green" }
    , { id = 103, x = 1250, y = 250, xlinkHref = "#csg", fill = "green" }
    , { id = 104, x = 1375, y = 250, xlinkHref = "#csg", fill = "green" }
    , { id = 111, x = 1250, y = 1250, xlinkHref = "#csr", fill = "red" }
    , { id = 112, x = 1375, y = 1250, xlinkHref = "#csr", fill = "red" }
    , { id = 113, x = 1250, y = 1375, xlinkHref = "#csr", fill = "red" }
    , { id = 114, x = 1375, y = 1375, xlinkHref = "#csr", fill = "red" }
    , { id = 121, x = 125, y = 1250, xlinkHref = "#csb", fill = "black" }
    , { id = 122, x = 250, y = 1250, xlinkHref = "#csb", fill = "black" }
    , { id = 123, x = 125, y = 1375, xlinkHref = "#csb", fill = "black" }
    , { id = 124, x = 250, y = 1375, xlinkHref = "#csb", fill = "black" }
    , { id = 131, x = 125, y = 125, xlinkHref = "#csy", fill = "yellow" }
    , { id = 132, x = 250, y = 125, xlinkHref = "#csy", fill = "yellow" }
    , { id = 133, x = 125, y = 250, xlinkHref = "#csy", fill = "yellow" }
    , { id = 134, x = 250, y = 250, xlinkHref = "#csy", fill = "yellow" }
    ]
