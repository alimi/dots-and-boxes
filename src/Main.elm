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
renderRows numberOfRows numberOfCells =
  if numberOfRows == 1 then
    [tr [] (renderCells numberOfRows numberOfCells)]
  else
    (renderRows (numberOfRows - 1) numberOfCells) ++ [tr [] (renderCells numberOfRows numberOfCells)]

renderCells : Int -> Int -> List (Html Msg)
renderCells rowNumber numberOfCells =
  if numberOfCells == 1 then
    [td [] [renderCell rowNumber numberOfCells]]
  else
    (renderCells rowNumber (numberOfCells - 1)) ++ [td [] [renderCell rowNumber numberOfCells]]

renderCell : Int -> Int -> Html Msg
renderCell x y =
  if (x % 2 == 1) && (y % 2 == 1) then
    text "*"
  else
    text (toString [x, y])

---- PROGRAM ----


main : Program Never Model Msg
main =
  Html.program
    { view = view
    , init = init
    , update = update
    , subscriptions = always Sub.none
    }
