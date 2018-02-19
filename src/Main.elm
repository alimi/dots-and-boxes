module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


---- MODEL ----


type alias Model =
  { numberOfRows : Int
  , numberOfColumns : Int
  , selectedDot : (Int, Int)
  , claimedDots : List (Int, Int)
  }


init : (Model, Cmd Msg)
init =
  (Model 3 3 (0,0) [], Cmd.none)



---- UPDATE ----


type Msg
  = DotSelected Int Int
  | DotUnselected Int Int
  | AdjacentDotSelected Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    DotSelected x y ->
      ({ model | selectedDot = (x,y) }, Cmd.none)

    DotUnselected x y ->
      ({ model | selectedDot = (0,0) }, Cmd.none)

    AdjacentDotSelected x y ->
      -- reset selected dot
      ({ model | claimedDots = model.claimedDots ++ [(x,y), model.selectedDot] }, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
  table [] (renderRows model.numberOfRows model.numberOfColumns model.selectedDot model.claimedDots)

renderRows : Int -> Int -> (Int, Int) -> List (Int, Int) -> List (Html Msg)
renderRows numberOfRows numberOfCells selectedDot claimedDots =
  if numberOfRows == 1 then
    [tr [] (renderCells numberOfRows numberOfCells selectedDot claimedDots)]
  else
    (renderRows (numberOfRows - 1) numberOfCells selectedDot claimedDots)
    ++ [tr [] (renderCells numberOfRows numberOfCells selectedDot claimedDots)]

renderCells : Int -> Int -> (Int, Int) -> List (Int, Int) -> List (Html Msg)
renderCells rowNumber numberOfCells selectedDot claimedDots =
  if numberOfCells == 1 then
    [renderCell rowNumber numberOfCells selectedDot claimedDots]
  else
    (renderCells rowNumber (numberOfCells - 1) selectedDot claimedDots)
    ++ [renderCell rowNumber numberOfCells selectedDot claimedDots]

renderCell : Int -> Int -> (Int, Int) -> List (Int, Int) -> Html Msg
renderCell x y selectedDot claimedDots =
  if isDot x y then
    renderDot x y selectedDot
  else if (x /= y) then
    if (List.member (x - 1, y) claimedDots) && (List.member (x + 1, y) claimedDots) then
      td [] [text "|"]
    else if (List.member (x, y - 1) claimedDots) && (List.member (x, y + 1) claimedDots) then
      td [] [text "-"]
    else
      td [] [text (toString [x, y])]
  else
    td [] [text (toString [x, y])]

renderDot : Int -> Int -> (Int, Int) -> Html Msg
renderDot x y selectedDot =
  if (x,y) == selectedDot then
    td [onClick (DotUnselected x y), class "dot selected"] [text "*"]
  else if isAdjacent x y selectedDot then
    td [onClick (AdjacentDotSelected x y), class "dot adjacent"] [text "*"]
  else
    td [onClick (DotSelected x y), class "dot"] [text "*"]

isDot : Int -> Int -> Bool
isDot x y =
  (x % 2 == 1) && (y % 2 == 1)

isAdjacent : Int -> Int -> (Int, Int) -> Bool
isAdjacent x y dot =
  if (x - 2 == Tuple.first dot) && y == Tuple.second dot then
    True
  else if (x + 2 == Tuple.first dot) && y == Tuple.second dot then
    True
  else if x == Tuple.first dot && (y - 2 == Tuple.second dot) then
    True
  else if x == Tuple.first dot && (y + 2 == Tuple.second dot) then
    True
  else
    False


---- PROGRAM ----


main : Program Never Model Msg
main =
  Html.program
    { view = view
    , init = init
    , update = update
    , subscriptions = always Sub.none
    }
