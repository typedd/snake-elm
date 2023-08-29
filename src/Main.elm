module Main exposing (..)

import Element exposing (Element, el, text, row, fill, width, height, rgb255, spacing, centerY, centerX, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font



-- MAIN


main =
  Element.layout []
      fieldRow

fieldRow : Element msg
fieldRow = 
  row [ width fill, height fill, spacing 1 ]
      [ cell
      , cell
      , cell
      , cell
      ]

cell : Element msg
cell = 
  el
        [ centerX, centerY,
          Background.color (rgb255 240 0 245)
        , Font.color (rgb255 255 255 255)
        , Border.rounded 3
        , padding 30
        ]
        (text "")



-- MODEL

-- type alias Model = List (List ())

-- init : Model
-- init =
--   [[],[],[]]


-- -- UPDATE


-- type Msg
--   = Model


-- update : Msg -> Model -> Model
-- update msg model = model



-- -- VIEW


-- view : Model -> Html Msg
-- view model =
--   div [] []
