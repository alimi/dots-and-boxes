module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


---- MODEL ----


type alias Model =
  { numberOfRows : Int
  , numberOfColumns : Int
  , selectedDot : (Int, Int)
  }


init : (Model, Cmd Msg)
init =
  (Model 3 3 (0,0), Cmd.none)



---- UPDATE ----


type Msg
  = DotSelected Int Int
  | DotUnselected Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    DotSelected x y ->
      ({ model | selectedDot = (x,y) }, Cmd.none)

    DotUnselected x y ->
      ({ model | selectedDot = (0,0) }, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
  table [] (renderRows model.numberOfRows model.numberOfColumns model.selectedDot)

renderRows : Int -> Int -> (Int, Int) ->List (Html Msg)
renderRows numberOfRows numberOfCells selectedDot =
  if numberOfRows == 1 then
    [tr [] (renderCells numberOfRows numberOfCells selectedDot)]
  else
    (renderRows (numberOfRows - 1) numberOfCells selectedDot)
    ++ [tr [] (renderCells numberOfRows numberOfCells selectedDot)]

renderCells : Int -> Int -> (Int, Int) -> List (Html Msg)
renderCells rowNumber numberOfCells selectedDot =
  if numberOfCells == 1 then
    [renderCell rowNumber numberOfCells selectedDot]
  else
    (renderCells rowNumber (numberOfCells - 1) selectedDot)
    ++ [renderCell rowNumber numberOfCells selectedDot]

renderCell : Int -> Int -> (Int, Int) -> Html Msg
renderCell x y selectedDot =
  if (x % 2 == 1) && (y % 2 == 1) then
    if (x,y) == selectedDot then
      td [onClick (DotUnselected x y), class "dot selected"] [text "*"]
    else
      td [onClick (DotSelected x y), class "dot"] [text "*"]
  else
    td [] [text (toString [x, y])]

---- PROGRAM ----


main : Program Never Model Msg
main =
  Html.program
    { view = view
    , init = init
    , update = update
    , subscriptions = always Sub.none
    }
