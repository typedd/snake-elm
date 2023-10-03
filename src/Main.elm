module Main exposing (..)

import Browser
import Element exposing (..)
import Html exposing (Html)
import Element.Background as Background
import Element.Border as Border
import Time
import Keyboard exposing (RawKey)



-- MAIN


main =
  Browser.element 
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions }



-- MODEL


type alias Model =
  { snake: List { x : Int, y : Int }
  , directHead: DirSnake
  }


init : () -> (Model, Cmd Msg)
init _ =
  ({snake = [{ x = 4, y = 4 }, { x = 4, y = 5 }, { x = 4, y = 6 }, {x = 4, y = 7}]
  , directHead = UP
  }, Cmd.none
  )



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



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      Tick _ ->
        let
          (dx, dy) =
            case model.directHead of
              UP -> 
                (0, if (isHeadLeaveTopLeft model) then -1 else 8)

              DOWN -> 
                (0, if (isHeadLeaveBottomRight model) then 1 else -8)

              LEFT -> 
                (if (isHeadLeaveTopLeft model) then -1 else 8, 0)

              RIGHT -> 
                (if (isHeadLeaveBottomRight model) then 1 else -8, 0)

          headPosition =
            case List.head model.snake of
              Just { x, y } -> { x = x + dx, y = y + dy }
              Nothing -> { x = 0, y = 0 }

          newSnake =
            headPosition :: List.take (List.length model.snake - 1) model.snake
        in
          ({ model | snake = newSnake }, Cmd.none)

      KeyDown key ->
        let
          newDirection =
            case Keyboard.rawValue key of
              "ArrowUp" ->
                UP

              "ArrowDown" ->
                DOWN

              "ArrowLeft" ->
                LEFT

              "ArrowRight" ->
                RIGHT

              _ ->
                model.directHead
        in
          (Debug.log (Debug.toString key)
            <|
              { model | directHead = newDirection }, Cmd.none)

      KeyUp _ ->
        (model, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  fieldDrow model.snake


fieldDrow : List { x : Int, y : Int } -> Html msg
fieldDrow snake = 
  layout
    []
    <|
    el [ centerX, centerY ]
      <|
      column [] (List.indexedMap (\yIndex _ -> fieldRow snake yIndex) (List.repeat 9 cell))


fieldRow : List { x : Int, y : Int } -> Int -> Element msg
fieldRow snake yIndex =
  Element.row [] (List.indexedMap (\xIndex _ -> pointSnake snake xIndex yIndex) (List.repeat 9 cell))


pointSnake : List { x : Int, y : Int } -> Int -> Int -> Element msg
pointSnake snake xIndex yIndex =
  if List.head snake == Just { x = xIndex, y = yIndex } then
    cell 0 100 0
  else if List.any (\pos -> pos.x == xIndex && pos.y == yIndex) (List.drop 1 snake) then
    cell 0 250 0
  else
    cell 240 0 245


cell : Int -> Int -> Int -> Element msg
cell r g b= 
  el
    [ centerX, centerY
    , Background.color (rgb255 r g b)
    , Border.rounded 3
    , padding 20
    ]
    Element.none


isHeadLeaveTopLeft : Model -> Bool
isHeadLeaveTopLeft model =
    case List.head model.snake of
        Just head -> ((head.y > 0) && (head.x > 0))
        Nothing -> False

isHeadLeaveBottomRight : Model -> Bool
isHeadLeaveBottomRight model =
    case List.head model.snake of
        Just head -> ((head.y < 8) && (head.x < 8))
        Nothing -> False


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Time.every 500 Tick
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    ]
