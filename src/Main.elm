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

type alias Model = List (List ())

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
    fieldRow

fieldRow : Element msg
fieldRow = 
  row [ width fill, height fill, spacing 1 ]
      [ cell
      , cell
      , cellSnake
      , cell
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