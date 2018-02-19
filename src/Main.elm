module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)


---- MODEL ----


type alias Model =
  { numberOfRows : Int
  , numberOfColumns : Int
  }


init : (Model, Cmd Msg)
init =
  (Model 3 3, Cmd.none)



---- UPDATE ----


type Msg
  = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  (model, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
  table [] (renderRows model.numberOfRows model.numberOfColumns)

renderRows : Int -> Int -> List (Html Msg)
renderRows numberOfRows numberOfColumns =
  if numberOfRows == 1 then
    [tr [] (renderCells numberOfRows numberOfColumns)]
  else
    (renderRows (numberOfRows - 1) numberOfColumns) ++ [tr [] (renderCells numberOfRows numberOfColumns)]

renderCells : Int -> Int -> List (Html Msg)
renderCells rowNumber numberOfColumns =
  if numberOfColumns == 1 then
    [td [] [text (toString [rowNumber, numberOfColumns])]]
  else
    (renderCells rowNumber (numberOfColumns - 1)) ++ [td [] [text (toString [rowNumber, numberOfColumns])]]


---- PROGRAM ----


main : Program Never Model Msg
main =
  Html.program
    { view = view
    , init = init
    , update = update
    , subscriptions = always Sub.none
    }
