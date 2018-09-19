import Array exposing (Array)
import Browser
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events

main = Browser.element {init = init, view = view, update = update, subscriptions = subscriptions}

-- TYPES
type State = Setup | PlayerA | PlayerB | PlayerA_Wins | PlayerB_Wins | Draw
type alias Board = Array Int
type alias Model = {state: State, board : Board}
type Msg = ClickMsg Int


-- CONSTANTS
numPitsPerPlayer = 6
numStonesPerPlayer = 4
numStones = numPitsPerPlayer*numStonesPerPlayer
numPits = 2*(numPitsPerPlayer + 1)
basePlayerA = numPitsPerPlayer
basePlayerB = numPits - 1
boardWidth = 1000
boardHeight = 640
boardWidth_ = String.fromInt boardWidth
boardHeight_ = String.fromInt boardHeight
colPlayerA = "red"
colPlayerB = "blue"


-- INIT
init : () -> (Model, Cmd Msg)
init _ =
  (updateSetup <| Model Setup (Array.repeat numPits 0), Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ClickMsg pit ->
      if getIsValidStartingPit pit model || (-1 == pit) then
        (updateClickMsg pit model, Cmd.none)
      else
        (model, Cmd.none)

updateSetup : Model -> Model
updateSetup model =
  let
    board1 = Array.repeat numPits numStonesPerPlayer
    board2 = Array.set numPitsPerPlayer 0 board1
    board3 = Array.set (2*numPitsPerPlayer + 1) 0 board2
  in
    {model | board = board3, state = PlayerA}

updateClickMsg : Int -> Model -> Model
updateClickMsg pit model =
  let
    isPlayerA = (model.state == PlayerA)
    stones = getStonesInPit pit model.board
    removeStoneBoard = Array.set pit 0 model.board
    (lastPit, addStoneBoard) = addStone isPlayerA (pit + 1) stones removeStoneBoard
    isLastStoneInBase = getIsBase lastPit

    isTakeOpponentsStones = (not isLastStoneInBase) && (1 == (getStonesInPit lastPit addStoneBoard)) && (isPlayerA == getIsPlayerA lastPit)
    takeOpponentsStonesBoard =
      if isTakeOpponentsStones then
        let
          opponentsPit = numPits - 2 - (remainderBy numPits lastPit)
        in
          if isPlayerA then
            moveStonesToBase basePlayerA opponentsPit addStoneBoard
          else
            moveStonesToBase basePlayerB opponentsPit addStoneBoard
      else
        addStoneBoard

    isGameOver = getIsGameOver takeOpponentsStonesBoard
    gameOverBoard =
      if isGameOver then
        let
          clearPlayerABoard = List.foldl (moveStonesToBase basePlayerA) takeOpponentsStonesBoard <| List.range 0 (numPitsPerPlayer - 1)
          clearPlayerBBoard = List.foldl (moveStonesToBase basePlayerB) clearPlayerABoard <| List.range (numPitsPerPlayer + 1) (numPits - 2)
        in
          clearPlayerBBoard
      else
        takeOpponentsStonesBoard

    nextState =
      if isGameOver then
        let
          scorePlayerA = getStonesInPit basePlayerA gameOverBoard
          scorePlayerB = getStonesInPit basePlayerB gameOverBoard
        in
          if scorePlayerA > scorePlayerB then
            PlayerA_Wins
          else if scorePlayerB > scorePlayerA then
            PlayerB_Wins
          else
            Draw
      else if isLastStoneInBase then
        model.state
      else if isPlayerA then
        PlayerB
      else
        PlayerA

  in
    {model | board = gameOverBoard, state = nextState}


-- VIEW
view : Model -> Html.Html Msg
view model =
  Html.div []
    [ Svg.svg
      [ Svg.Attributes.width boardWidth_
      , Svg.Attributes.height boardHeight_
      , Svg.Attributes.viewBox ("0 0 " ++ boardWidth_ ++ " " ++ boardHeight_)
      ]
      [ viewBoard model ]
    ]

viewPit : Int -> Int -> Svg Msg
viewPit i v =
  let
    isBase = getIsBase i
    isPlayerA = getIsPlayerA i
    r = if isBase then 80 else 45
    cx =
      if isBase then
        if isPlayerA then
          700
        else
          100
      else if isPlayerA then
        150 + 100 * (getPlayerPit i)
      else
        150 + 100 * (numPitsPerPlayer - 1 - (getPlayerPit i))
    cy =
      if isBase then
        220
      else if isPlayerA then
        380
      else
        60
    col_ = if isPlayerA then colPlayerA else colPlayerB
    textCx = cx
    textCy = cy + r//4
    textRot = if isPlayerA then 0 else 180
    lineX1 = cx - r//3
    lineX2 = cx + r//3
    lineY1 = cy + (if isPlayerA then r//3 else -r//3)
    lineY2 = lineY1

    r_ = String.fromInt r
    cx_ = String.fromInt cx
    cy_ = String.fromInt cy
    v_ = String.fromInt v
    textCx_ = String.fromInt textCx
    textCy_ = String.fromInt textCy
    textRot_ = String.fromInt textRot
    lineX1_ = String.fromInt lineX1
    lineX2_ = String.fromInt lineX2
    lineY1_ = String.fromInt lineY1
    lineY2_ = String.fromInt lineY2
    textTransform_ = "rotate(" ++ textRot_ ++ " " ++ cx_ ++ "," ++ cy_ ++ ")"
  in
    Svg.g
      [ Svg.Attributes.id (String.fromInt i)
      , Svg.Events.onClick (ClickMsg i)
      ]
      [ Svg.circle
        [ Svg.Attributes.cx cx_
        , Svg.Attributes.cy cy_
        , Svg.Attributes.r r_
        , Svg.Attributes.fill "white"
        , Svg.Attributes.stroke col_
        ] []
      , Svg.text_
        [ Svg.Attributes.x textCx_
        , Svg.Attributes.y textCy_
        , Svg.Attributes.fontSize r_
        , Svg.Attributes.textAnchor "middle"
        , Svg.Attributes.transform textTransform_
        , Svg.Attributes.stroke col_
        , Svg.Attributes.fill col_
        ] [Svg.text v_]
      , Svg.line
        [ Svg.Attributes.x1 lineX1_
        , Svg.Attributes.y1 lineY1_
        , Svg.Attributes.x2 lineX2_
        , Svg.Attributes.y2 lineY2_
        , Svg.Attributes.stroke col_
        ] []
      ]

viewState : Model -> Svg Msg
viewState model =
  let
    cx = 400
    cy = 220
    cx_ = String.fromInt cx
    cy_ = String.fromInt cy
    settingsDefault = ("black", "rotate(0)")
    settingsA = (colPlayerA, "rotate(0)")
    settingsB = (colPlayerB, "rotate(180 " ++ cx_ ++ "," ++ cy_ ++ ")")
    ((col_, rot_), str_) =
      case model.state of
        Setup -> (settingsDefault, "Setup")
        PlayerA -> (settingsA, "Red")
        PlayerB -> (settingsB, "Blue")
        PlayerA_Wins -> (settingsA, "Red Wins")
        PlayerB_Wins -> (settingsB, "Blue Wins")
        Draw -> (settingsDefault, "Draw")
  in
    Svg.g []
      [ Svg.text_
        [ Svg.Attributes.x cx_
        , Svg.Attributes.y cy_
        , Svg.Attributes.fontSize "40"
        , Svg.Attributes.textAnchor "middle"
        , Svg.Attributes.stroke col_
        , Svg.Attributes.fill col_
        , Svg.Attributes.transform rot_
        ]
        [ Svg.text str_ ]
      ]

viewBoard : Model -> Svg Msg
viewBoard model =
  Svg.g [] ((::) (viewState model) <| Array.toList <| Array.indexedMap viewPit model.board)


-- HELPER
getStonesInPit : Int -> Array Int -> Int
getStonesInPit i array =
  Maybe.withDefault 0 <| Array.get i array

getIsBase i = (getPlayerPit i) == numPitsPerPlayer
getIsPlayerA i = i < (numPitsPerPlayer + 1)
getIsPlayerB i = not <| getIsPlayerA i
getPlayerPit i = remainderBy (numPitsPerPlayer + 1) i
getIsGameOver board =
  (0 == (Array.foldl (\v x -> x + v) 0 <| Array.slice 0 numPitsPerPlayer board)) ||
  (0 == (Array.foldl (\v x -> x + v) 0 <| Array.slice (-numPitsPerPlayer - 1) -1 board))

addStone : Bool -> Int -> Int -> Array Int -> (Int, Array Int)
addStone isPlayerA pit stonesToAdd board =
  let
    i = remainderBy numPits pit
    isSkip =
      (isPlayerA && (getIsPlayerB i && getIsBase i)) ||
      ((not isPlayerA) && (getIsPlayerA i && getIsBase i))
    v = getStonesInPit i board
    newBoard =
      if isSkip then
        board
      else
        Array.set i (v + 1) board
    newStonesToAdd =
      if isSkip then
        stonesToAdd
      else
        stonesToAdd - 1
  in
    if stonesToAdd == 0 then
      (remainderBy numPits (pit - 1), board)
    else
      addStone isPlayerA (pit + 1) newStonesToAdd newBoard

moveStonesToBase : Int -> Int -> Array Int -> Array Int
moveStonesToBase dest src board =
  let
    srcStones = getStonesInPit src board
    destStones = getStonesInPit dest board
    board2 = Array.set src 0 board
    board3 = Array.set dest (srcStones + destStones) board2
  in
    board3

getIsValidStartingPit : Int -> Model -> Bool
getIsValidStartingPit pit model =
  if getIsBase pit then
    False
  else if (getStonesInPit pit model.board) == 0 then
    False
  else if getIsPlayerA pit then
    model.state == PlayerA
  else
    model.state == PlayerB
