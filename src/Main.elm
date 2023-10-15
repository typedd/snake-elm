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
  , directHead: DirSnake
  , starterPage: Bool
  }

--initBerries : Int -> List Berry
--initBerries countBerries =
--    [{x = 2, y = 7}, {x = 6, y = 4}, {x = 0, y = 0}]
    

init : () -> (Model, Cmd Msg)
init _ =
  ({snake = [{ x = 4, y = 4 }, { x = 4, y = 5 }, { x = 4, y = 6 }, {x = 4, y = 7}]
  , berries = []
  , directHead = UP
  , starterPage = True
  }, Cmd.none
  )

randomGenerator : Random.Generator (Int, Int)
randomGenerator =
  Random.pair (Random.int 0 8) (Random.int 0 8)

-- UPDATE

type DirSnake
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
      Tick _ ->
        let
          (dx, dy) =
            case model.directHead of
              UP -> 
                (0, if (isHeadLeaveTop model) then -1 else 8)

              DOWN -> 
                (0, if (isHeadLeaveBottom model) then 1 else -8)

              LEFT -> 
                (if (isHeadLeaveLeft model) then -1 else 8, 0)

              RIGHT -> 
                (if (isHeadLeaveRight model) then 1 else -8, 0)

          headPosition =
            case List.head model.snake of
              Just { x, y } -> { x = x + dx, y = y + dy }
              Nothing -> { x = 0, y = 0 } 

          ateBerry =
            List.any (\berry -> berry.x == headPosition.x && berry.y == headPosition.y) model.berries  

          --newBerries = removeBerry model.berries headPosition.x headPosition.y

          newSnake =
            if ateBerry then (headPosition :: List.take (List.length model.snake) model.snake)
            else (headPosition :: List.take (List.length model.snake - 1) model.snake)

          cmd : Cmd Msg
          cmd = 
            if (List.length newBerries < 4) then Random.generate RandomBerry (Random.list 81 randomGenerator)
            else Cmd.none

        in
          if model.starterPage == True then (model, cmd) 
          else (
            { model | snake = newSnake }
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
        ({ model | starterPage = False }, Cmd.none)

      RandomBerry listCoord ->
        let
          coords : List (Int, Int)
          coords = listCoord

          headPosition =
            case List.head model.snake of
              Just { x, y } -> { x = x, y = y }
              Nothing -> { x = 0, y = 0 } 

          listBerries : List Berry
          listBerries = removeBerry model.berries headPosition.x headPosition.y ++ List.map (\(x, y) -> { x = x, y = y }) coords

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

-- VIEW


view : Model -> Html Msg
view model =
  if model.starterPage == True then gameStart else fieldDrow model.snake model.berries


gameStart : Html msg
gameStart = 
  layout
    []
    <|
    el [ centerX, centerY ]
      <|
      column [spacing 10] [title, subTitle]

title : Element msg
title =
  el [ Background.color (rgb255 0 255 0)
    , Element.paddingXY 50 2
    , Font.bold
    , Font.size 80
    ]
    (text "SNAKE ELM")

subTitle : Element msg
subTitle =
  el [ centerX, centerY
    , Background.color (rgb255 0 0 255)
    , Element.paddingXY 30 5
    , Font.bold
    , Font.size 40
    , Border.rounded 3
    ]
    (text "Press any key")


fieldDrow : List { x : Int, y : Int } -> List { x : Int, y : Int } -> Html msg
fieldDrow snake berries = 
  layout
    []
    <|
    el [ centerX, centerY ]
      <|
      column [] (List.indexedMap (\yIndex _ -> fieldRow snake berries yIndex) (List.repeat 9 cell))


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

isHeadLeaveTop : Model -> Bool
isHeadLeaveTop model =
    case List.head model.snake of
        Just head -> (head.y > 0)
        Nothing -> False

isHeadLeaveLeft : Model -> Bool
isHeadLeaveLeft model =
    case List.head model.snake of
        Just head -> (head.x > 0)
        Nothing -> False

isHeadLeaveBottom : Model -> Bool
isHeadLeaveBottom model =
    case List.head model.snake of
        Just head -> (head.y < 8)
        Nothing -> False

isHeadLeaveRight : Model -> Bool
isHeadLeaveRight model =
    case List.head model.snake of
        Just head -> (head.x < 8)
        Nothing -> False

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ if model.starterPage == False then Time.every 500 Tick else Sub.none
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    ]
