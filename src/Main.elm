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
  { headPosition : {x : Int, y : Int}
  , directHead: DirSnake
  }


init : () -> (Model, Cmd Msg)
init _ =
  ({headPosition = {x = 4, y = 4}
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
          newHeadPosition =
            case model.directHead of
              UP ->
                if model.headPosition.y <= 0 then {x = model.headPosition.x, y = 8}
                else {x = model.headPosition.x, y = model.headPosition.y - 1}

              DOWN ->
                if model.headPosition.y >= 8 then {x = model.headPosition.x, y = 0}
                else {x = model.headPosition.x, y = model.headPosition.y + 1}

              LEFT ->
                if model.headPosition.x <= 0 then {x = 8, y = model.headPosition.y}
                else {x = model.headPosition.x - 1, y = model.headPosition.y}

              RIGHT ->
                if model.headPosition.x >= 8 then {x = 0, y = model.headPosition.y}
                else {x = model.headPosition.x + 1, y = model.headPosition.y}

        in
          ({model | headPosition = newHeadPosition}
          , Cmd.none
          )

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
  fieldDrow model.headPosition.x model.headPosition.y


fieldDrow : Int -> Int -> Html msg
fieldDrow xSnake ySnake =
  layout
    []
    <|
      el [ centerX, centerY ]
      <|
        column [] (List.indexedMap (\yIndex _ -> fieldRow xSnake ySnake yIndex) (List.repeat 9 cell))


fieldRow : Int -> Int -> Int -> Element msg
fieldRow xSnake ySnake yIndex =
  Element.row [] (List.indexedMap (\xIndex _ -> pointSnake xSnake ySnake xIndex yIndex) (List.repeat 9 cell))


pointSnake : Int -> Int -> Int -> Int -> Element msg
pointSnake xSnake ySnake xIndex yIndex =
  if (xSnake == xIndex && ySnake == yIndex ) then cellSnake 
  else cell


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Time.every 500 Tick
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    ]
