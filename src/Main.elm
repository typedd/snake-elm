module Main exposing (..)

import Browser
import Element exposing (..)
import Html exposing (Html)
import Element.Background as Background
import Element.Border as Border
import Time



-- MAIN


main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL

type alias Model =
  { field: List (List ())
  , currentHeadPosition: Int
  }


init : () -> (Model, Cmd Msg)
init flag =
  ( { field = [[],[],[]]
  , currentHeadPosition = 0
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
            if model.currentHeadPosition == 3 then 0
            else model.currentHeadPosition + 1
        in
        ({model | currentHeadPosition = newHeadPosition}
        , Cmd.none
        )



-- VIEW


view : Model -> Html Msg
view model =
  layout
    []
    (fieldRow model.currentHeadPosition)

fieldRow : Int -> Element msg
fieldRow currentHeadPosition =
  row [ width fill, height fill, spacing 1 ]
      [ if currentHeadPosition == 0 then cellSnake else cell
      , if currentHeadPosition == 1 then cellSnake else cell
      , if currentHeadPosition == 2 then cellSnake else cell
      , if currentHeadPosition == 3 then cellSnake else cell
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


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick    

-- every : Float -> (Posix -> msg) -> Sub msg
-- 1000 : Float
-- Tick : Posix -> msg