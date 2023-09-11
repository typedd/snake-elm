module Main exposing (..)


import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Browser
import Html exposing (Html)
import Html exposing (div)



-- MAIN


main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

--MODEL

type alias Model = Int

init : () -> (Model, Cmd msg)
init _ =
  (0, Cmd.none)


-- UPDATE


update : msg -> Model -> (Model, Cmd msg)
update msg model =
    (model, Cmd.none)



-- VIEW


view : Model -> Html msg
view _ =
  layout
    []
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
        , Border.rounded 3
        , padding 30
        ]
        Element.none

subscriptions : Model -> Sub msg
subscriptions model =
  Sub.none