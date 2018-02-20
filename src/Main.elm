module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


---- MODEL ----


type alias Model =
  { numberOfRows : Int
  , numberOfColumns : Int
  , selectedDot : (Int, Int)
  , connections : List (Int, Int)
  , players : List String
  , currentPlayer : String
  }


init : (Model, Cmd Msg)
init =
  (Model 3 3 (0,0) [] ["Player 1", "Player 2"] "Player 1", Cmd.none)



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
      let
        connection = connectionBetween (x, y) model.selectedDot
      in
        ({ model
         | connections = connection::model.connections
         , selectedDot = (0,0)
         , currentPlayer = nextPlayer model
         }
        , Cmd.none
        )

connectionBetween : (Int, Int) -> (Int, Int) -> (Int, Int)
connectionBetween adjacentDot selectedDot =
  let
    adjacentX = Tuple.first adjacentDot
    adjacentY = Tuple.second adjacentDot
    selectedX = Tuple.first selectedDot
    selectedY = Tuple.second selectedDot
  in
    if adjacentX == selectedX then
      if adjacentY > selectedY then
        (selectedX, selectedY + 1)
      else
        (adjacentX, adjacentY + 1)
    else
      if adjacentX > selectedX then
        (selectedX + 1, selectedY)
      else
        (adjacentX + 1, adjacentY)

nextPlayer : Model -> String
nextPlayer model =
  List.filter (\player -> player /= model.currentPlayer) model.players
    |> List.head
    |> Maybe.withDefault "Whoops"


---- VIEW ----


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text model.currentPlayer ]
    , table [] (renderRows model.numberOfRows model)
    ]

renderRows : Int -> Model -> List (Html Msg)
renderRows numberOfRows model =
  if numberOfRows == 1 then
    [tr [] (renderCells numberOfRows model.numberOfColumns model)]
  else
    (renderRows (numberOfRows - 1) model)
    ++ [tr [] (renderCells numberOfRows model.numberOfColumns model)]

renderCells : Int -> Int -> Model -> List (Html Msg)
renderCells rowNumber numberOfCells model =
  if numberOfCells == 1 then
    [renderCell rowNumber numberOfCells model]
  else
    (renderCells rowNumber (numberOfCells - 1) model)
    ++ [renderCell rowNumber numberOfCells model]

renderCell : Int -> Int -> Model -> Html Msg
renderCell x y model =
  if isDot x y then
    renderDot x y model.selectedDot
  else if List.member (x,y) model.connections then
    renderConnection x
  else
    td [] []

renderDot : Int -> Int -> (Int, Int) -> Html Msg
renderDot x y selectedDot =
  if (x,y) == selectedDot then
    td [onClick (DotUnselected x y), class "dot selected"] [text "*"]
  else if isAdjacent x y selectedDot then
    td [onClick (AdjacentDotSelected x y), class "dot adjacent"] [text "*"]
  else
    td [onClick (DotSelected x y), class "dot"] [text "*"]

renderConnection : Int -> Html Msg
renderConnection x =
  if x % 2 == 1 then
    td [] [text "--"]
  else
    td [] [text "|"]

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
