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
  { field: List (List ())
  , xHeadPosition: Int
  , yHeadPosition: Int
  , directHead: DirSnake
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { field = [[],[],[]]
  , xHeadPosition = 4
  , yHeadPosition = 4
  , directHead = Up
  }, Cmd.none
  )


-- UPDATE

type DirSnake
    = Up
    | Down
    --| Left
    --| Right

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
              Up ->
                if model.yHeadPosition <= 0 then 8
                else model.yHeadPosition - 1
              Down ->
                if model.yHeadPosition >= 8 then 0
                else model.yHeadPosition + 1
        in
          ({model | yHeadPosition = newHeadPosition
                    }
          , Cmd.none
          )
      KeyDown _ -> 
        ({model | directHead = Down
                --, yHeadPosition = model.yHeadPosition + 1
                    }
          , Cmd.none

        )
      KeyUp _ -> 
        ({model | directHead = Up
                --, yHeadPosition = model.yHeadPosition - 1
                    }
          , Cmd.none

        )  

      --any_other_msg ->        
        --Debug.log (Debug.toString any_other_msg) 
          --<| 
            --(model, 
            --Cmd.none)



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
  Sub.batch
    [ Time.every 1000 Tick
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    ]
  
