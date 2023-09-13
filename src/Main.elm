module Main exposing (..)

import Browser
import Element exposing (..)
import Html exposing (Html)
import Element.Background as Background
import Element.Border as Border
import Time
import Task 



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Model = 
  { --field: List (List ())
  --,  
    x : Int
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 1 
  , Cmd.none
  ) 
 
 
 -- [[],[],[]]


-- UPDATE


type Msg
  = Tick Time.Posix | NewPosition (Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Tick ->
      ( model.x + 1
      , Cmd.none
      )
    NewPosition (x) ->
      ( Model x
      , Cmd.none
      )



-- VIEW


view : Model -> Html Msg
view model =
  layout
    []
    (fieldRow model.x)
            

fieldRow : Int -> Element msg
fieldRow x = 
  row [ width fill, height fill, spacing 1 ]
      [ if x == 1 then cellSnake else cell
      , if x == 2 then cellSnake else cell
      , if x == 3 then cellSnake else cell
      , if x == 4 then cellSnake else cell
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
