module Main exposing (..)

import Browser
import Element exposing (..)
import Html exposing (Html)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Time
import Keyboard exposing (RawKey, Key)
import Random exposing (..)



-- MAIN


main =
  Browser.element 
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions }



-- MODEL

type alias Berry =
  { x : Int
  , y : Int
  }


type alias Model =
  { snake: List { x : Int, y : Int }
  , berries: List Berry
  , directHead: DirectionSnake
  , starterPage: Bool
  , gameOverPage: Bool
  , score: Int
  , level: Int
  , record: Int
  , gameTime: Time.Posix
  , seconds: Int
  }
 

init : () -> (Model, Cmd Msg)
init _ =
  let
    randomCmd = Random.generate RandomBerry (Random.list 10 randomGenerator)
  in
    ({snake = [{ x = 4, y = 4 }, { x = 4, y = 5 }, { x = 4, y = 6 }, {x = 4, y = 7 }]
    , berries = []
    , directHead = UP
    , starterPage = True
    , gameOverPage = False
    , score = 0
    , level = 1
    , record = 0
    , gameTime = Time.millisToPosix 0
    , seconds = 0
  }, randomCmd
  )


randomGenerator : Random.Generator (Int, Int)
randomGenerator =
  Random.pair (Random.int 0 8) (Random.int 0 8)



-- UPDATE


type DirectionSnake
    = UP
    | DOWN
    | LEFT
    | RIGHT


type Msg
  = Tick Time.Posix
  | KeyDown RawKey
  | KeyUp RawKey
  | RandomBerry (List (Int, Int))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      Tick time ->
        let

          newGameTime = time

          newSeconds = 
            if (Time.posixToMillis time // 1000 /= Time.posixToMillis model.gameTime // 1000)
              then (model.seconds + 1)
            else model.seconds

          (dx, dy) =
            case model.directHead of
              UP -> (0, -1)

              DOWN -> (0, 1)

              LEFT -> (-1, 0)

              RIGHT -> (1, 0)

          headPosition =
            case List.head model.snake of
              Just { x, y } -> { x = x + dx, y = y + dy }
              Nothing -> { x = 0, y = 0 } 

          ateBerry =
            List.any (\berry -> berry.x == headPosition.x && berry.y == headPosition.y) model.berries

          -- Увеличиваем счет при съедании ягоды 
          newScore =
            if ateBerry then model.score + 1 else model.score 

          newBerries = removeBerry model.berries headPosition.x headPosition.y

          newSnake =
            if ateBerry then (headPosition :: List.take (List.length model.snake) model.snake)
            else (headPosition :: List.take (List.length model.snake - 1) model.snake)

          --повышаем level
          newLevel =
            if ateBerry && (modBy 5 (model.score + 1) == 0) then
              model.level + 1
            else
              model.level

          cmd : Cmd Msg
          cmd = 
            if (List.length newBerries < 4) then Random.generate RandomBerry (Random.list 81 randomGenerator)
            else Cmd.none

          newRecord : Int
          newRecord = 
            if (model.record < newScore) then newScore
            else model.record

        in
          if model.starterPage == True then (model, Cmd.none) 
          else 
            if isGameOver model.snake then (
              { model | gameOverPage = True, record = newRecord }
              , Cmd.none)
            else (
              { model | snake = newSnake, score = newScore, level = newLevel, gameTime = newGameTime, seconds = newSeconds }
              , cmd)

      KeyDown key ->
        let
          lastDirection = model.directHead
          newDirection =
            case Keyboard.rawValue key of
              "ArrowUp" ->
                if lastDirection == DOWN then DOWN else UP

              "ArrowDown" ->
                if lastDirection == UP then UP else DOWN

              "ArrowLeft" ->
                if lastDirection == RIGHT then RIGHT else LEFT

              "ArrowRight" ->
                if lastDirection == LEFT then LEFT else RIGHT

              _ ->
                model.directHead
        in
          (Debug.log (Debug.toString key)
            <|
              { model | directHead = newDirection }, Cmd.none)

      KeyUp _ ->
        if model.gameOverPage == True then
          let
            randomCmd = Random.generate RandomBerry (Random.list 10 randomGenerator)
          in
            ({snake = [{ x = 4, y = 4 }, { x = 4, y = 5 }, { x = 4, y = 6 }, {x = 4, y = 7}]
            , berries = []
            , directHead = UP
            , starterPage = True
            , gameOverPage = False
            , score = 0
            , level = 1
            , record = model.record
            , gameTime = Time.millisToPosix 0
            , seconds = 0
            }, randomCmd
            )
        else
          ({ model | starterPage = False }, Cmd.none)

      RandomBerry listCoord ->
        let
          coords : List (Int, Int)
          coords = listCoord

          listBerries : List Berry
          listBerries = model.berries ++ List.map (\(x, y) -> { x = x, y = y }) coords

          newlistBerries = List.take 3 (List.filter (\berry -> not (isBerryOnSnake berry model.snake)) listBerries)

        in
          ({ model | berries = newlistBerries }, Cmd.none)


--удаляем съеденную ягоду из списка ягод на поле
removeBerry : List Berry -> Int -> Int -> List Berry
removeBerry berries xToRemove yToRemove =
  List.filter (\berry -> berry.x /= xToRemove || berry.y /= yToRemove) berries


--проверяем попадает ли ягода на змею
isBerryOnSnake : Berry -> List { x : Int, y : Int } -> Bool
isBerryOnSnake berry snake =
  List.any (\segment -> segment.x == berry.x && segment.y == berry.y) snake


--проверяем врезалась ли змейка в стену или съела себя 
isGameOver : List { x : Int, y : Int } -> Bool
isGameOver snake =
    case List.head snake of
        Just head ->
          if (head.x < 0 || head.x > 8 || head.y < 0 || head.y > 8) then True 
          else 
            if List.any (\segment -> segment.x == head.x && segment.y == head.y) (List.drop 1 snake) then
              True
            else
              False
        Nothing ->
            True



-- VIEW


view : Model -> Html Msg
view model =
  if model.starterPage == True then viewGameStart 
  else 
    if model.gameOverPage == True then viewGameOver model.score model.record
    else
      fieldDraw model.snake model.berries model.score model.level model.seconds model.record


viewGameStart : Html msg
viewGameStart = 
  layout
    []
    <|
    el [ centerX, centerY ]
      <|
      column [spacing 10] 
      [ titleSnakeElm
      , titlePressAnyKey
      ]


viewGameOver : Int -> Int -> Html msg
viewGameOver score record= 
  layout
    []
    <|
    el [ centerX, centerY ]
      <|
      column [spacing 10] 
        [ titleGameOver
        , titleScore score
        , titleRecord record
        , titlePressAnyKey
        ]


--titleScore : 
titleScore : Int -> Element msg
titleScore score =
    el [ centerX, centerY
      , Element.paddingXY 50 2
      , Font.bold
      , Font.size 30
      ]
    (text ("SCORE: " ++ (String.fromInt score)))


--titleLevel : 
titleLevel : Int -> Element msg
titleLevel level =
    el [ centerX, centerY
      , Element.paddingXY 50 2
      , Font.bold
      , Font.size 30
      ]
    (text ("LEVEL: " ++ (String.fromInt level)))


--titleTime : 
titleTimeGame : Int -> Element msg
titleTimeGame seconds = 
    el [ centerX, centerY
      , Element.paddingXY 50 2
      , Font.bold
      , Font.size 30
      ]
    (text 
      ("Time: "
      ++ String.padLeft 2 '0' (String.fromInt (seconds // 60))
      ++ ":"
      ++ String.padLeft 2 '0' (String.fromInt (modBy 60 seconds))
      )
    )


--titleRecord 
titleRecord : Int -> Element msg
titleRecord record =
    el [ centerX, centerY
      , Background.color (rgb255 165 245 65)
      , Element.paddingXY 50 2
      , Font.bold
      , Font.size 30
      ]
    (text ("RECORD: " ++ (String.fromInt record)))


titleSnakeElm : Element msg
titleSnakeElm =
  el [ Background.color (rgb255 0 255 0)
    , Element.paddingXY 50 2
    , Font.bold
    , Font.size 80
    ]
    (text "SNAKE ELM")


titlePressAnyKey : Element msg
titlePressAnyKey =
  el [ centerX, centerY
    , Background.color (rgb255 0 0 255)
    , Element.paddingXY 30 5
    , Font.bold
    , Font.size 40
    , Font.color (rgb255 255 255 255)
    , Border.rounded 5
    ]
    (text "Press any key")


titleGameOver : Element msg
titleGameOver =
  el [ Background.color (rgb255 255 0 0)
    , Element.paddingXY 50 2
    , Font.bold
    , Font.color (rgb255 255 255 255)
    , Font.size 70
    ]
    (text "GAME OVER")


fieldDraw : List { x : Int, y : Int } -> List { x : Int, y : Int } -> Int -> Int -> Int -> Int -> Html msg
fieldDraw snake berries score level seconds record= 
  layout
    []
    <|
    el [ centerX, centerY ]
      <|
      column []
        [ row [spacing 10]
            [ column [] 
              [titleSnakeElm
              , titleScore score
              , titleLevel level
              , titleTimeGame seconds
              , titleRecord record]
            , column [] (List.indexedMap (\yIndex _ -> fieldRow snake berries yIndex) (List.repeat 9 cell))
            ]
        ]


fieldRow : List { x : Int, y : Int } -> List { x : Int, y : Int } -> Int -> Element msg
fieldRow snake berries yIndex =
  Element.row [] (List.indexedMap (\xIndex _ -> pointSnake snake berries xIndex yIndex) (List.repeat 9 cell))


pointSnake : List { x : Int, y : Int } -> List { x : Int, y : Int } -> Int -> Int -> Element msg
pointSnake snake berries xIndex yIndex =
  if List.head snake == Just { x = xIndex, y = yIndex } then
    cellHeadSnake
  else if List.any (\pos -> pos.x == xIndex && pos.y == yIndex) (List.drop 1 snake) then
    cellSnake 
    else if List.any (\pos -> pos.x == xIndex && pos.y == yIndex) berries then 
      cellBerry
    else
      cell


cell : Element msg
cell = 
  el
    [ centerX, centerY
    , Background.color (rgb255 240 0 245)
    , Border.rounded 3
    , padding 20
    ]
    Element.none


cellSnake : Element msg
cellSnake = 
  el
    [ centerX, centerY
    , Background.color (rgb255 0 250 0)
    , Border.rounded 3
    , padding 20
    ]
    Element.none

cellHeadSnake : Element msg
cellHeadSnake = 
  el
    [ centerX, centerY
    , Background.color (rgb255 0 100 0)
    , Border.rounded 3
    , padding 20
    ]
    Element.none


cellBerry : Element msg
cellBerry = 
  el
    [ centerX, centerY
    , Background.color (rgb255 250 0 0)
    , Border.rounded 3
    , padding 20
    ]
    Element.none



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ if model.starterPage == False then Time.every (calculateTickInterval model) Tick else Sub.none
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    ]

calculateTickInterval : Model -> Float
calculateTickInterval model = 
  if model.starterPage || model.level <= 1 then
    500
  else
    500 - (toFloat(model.level) * 100)