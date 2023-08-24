module Main exposing (..)

import Browser
import Html exposing (Html, div)



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
  div [] []
