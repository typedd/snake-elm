module Main exposing (..)

import Browser
import Element exposing (..)
import Html exposing (Html)
import Element.Background as Background
import Element.Border as Border
import Time



-- MAIN


main =
  Browser.element 
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions }



-- MODEL

type alias Model =
  { field: List (List ())
  , xHeadPosition: Int
  , yHeadPosition: Int
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { field = [[],[],[]]
  , xHeadPosition = 4
  , yHeadPosition = 4
  }, Cmd.none
  )


-- UPDATE


type Msg
  = Tick Time.Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      Tick _ -> 
        let
          newHeadPosition =
            if model.yHeadPosition == 0 then 8
            else model.yHeadPosition - 1
        in
        ({model | yHeadPosition = newHeadPosition
                  }
        , Cmd.none
        )



-- VIEW


view : Model -> Html Msg
view model =
  fieldDrow model.xHeadPosition model.yHeadPosition

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
  if (xSnake == xIndex && ySnake == yIndex ) then cellSnake else cell

cell : Element msg
cell = 
  el
        [ centerX, centerY,
          Background.color (rgb255 240 0 245)
        , Border.rounded 3
        , padding 20
        ]
        Element.none

cellSnake : Element msg
cellSnake = 
  el
        [ centerX, centerY,
          Background.color (rgb255 0 250 0)
        , Border.rounded 3
        , padding 20
        ]
        Element.none


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Time.every 1000 Tick    
