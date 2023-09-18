module Main exposing (..)

import Browser
import Element exposing (..)
import Html exposing (Html)
import Element.Background as Background
import Element.Border as Border



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Model =
  { field: List (List ())
  , currentHeadPosition: Int
  }


init : Model
init =
  [[],[],[]]


-- UPDATE


type Msg
  = Model


update : Msg -> Model -> Model
update msg model = model



-- VIEW


view : Model -> Html Msg
view model =
  layout
    []
    (fieldRow model.currentHeadPosition)

fieldRow : Int -> Element msg
fieldRow currentHeadPosition = 
  row [ width fill, height fill, spacing 1 ]
      [ if currentHeadPosition == 0 cellSnake else cell
      , if currentHeadPosition == 1 cellSnake else cell
      , if currentHeadPosition == 2 cellSnake else cell
      , if currentHeadPosition == 3 cellSnake else cell
      ]

cell : Element msg
cell = 
  el
        [ centerX, centerY,
          Background.color (rgb255 240 0 245)
        , Border.rounded 3
        , padding 30
        ]
        Element.none

cellSnake : Element msg
cellSnake = 
  el
        [ centerX, centerY,
          Background.color (rgb255 0 250 0)
        , Border.rounded 3
        , padding 30
        ]
        Element.none