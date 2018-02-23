module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


---- MODEL ----

type alias Box =
  { sides : List (Int, Int)
  , center : (Int, Int)
  , owner : String
  }

type alias Model =
  { numberOfRows : Int
  , numberOfColumns : Int
  , selectedDot : (Int, Int)
  , connections : List (Int, Int)
  , players : List String
  , currentPlayer : String
  , boxes : List Box
  }


init : (Model, Cmd Msg)
init =
  (Model 3 3 (0,0) [] ["Player 1", "Player 2"] "Player 1" [], Cmd.none)



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
        newBoxes = findNewBoxes connection model
      in
        ({ model
         | connections = connection::model.connections
         , selectedDot = (0,0)
         , currentPlayer = nextPlayer model
         , boxes = model.boxes ++ newBoxes
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

findNewBoxes : (Int, Int) -> Model -> List Box
findNewBoxes newConnection model =
  let
    boxes = findAllBoxes model.numberOfRows model.numberOfColumns
  in
    List.filter (hasThreeCompleteSides model.connections) boxes
      |> List.filter (\box -> List.member newConnection box.sides)
      |> List.map (\box -> { box | owner = model.currentPlayer })

hasThreeCompleteSides : List (Int, Int) -> Box -> Bool
hasThreeCompleteSides connections box =
  let
    completeSides = List.filter (\connection -> List.member connection box.sides) connections
  in
    List.length completeSides == 3

findAllBoxes : Int -> Int -> List Box
findAllBoxes numberOfRows numberOfColumns =
  let
    evenXs = List.range 1 numberOfRows |> List.filter (\x -> x % 2 == 0)
    evenYs = List.range 1 numberOfColumns |> List.filter (\y -> y % 2 == 0)
    centers = allPairs evenXs evenYs
  in
    List.map buildBox centers

buildBox : (Int, Int) -> Box
buildBox center =
  let
    x = Tuple.first center
    y = Tuple.second center
  in
    Box [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] center ""

allPairs : List Int -> List Int -> List (Int, Int)
allPairs xs ys =
  case xs of
    [] ->
      []

    (x::remainingXs) ->
      List.map (\y -> (x,y)) ys ++ allPairs remainingXs ys


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
  else if List.member (x,y) (List.map (\box -> box.center) model.boxes) then
    renderOwnedBox x y model.boxes
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

renderOwnedBox : Int -> Int -> List Box -> Html Msg
renderOwnedBox x y boxes =
  let
    boxesAroundCenter = List.filter (\box -> box.center == (x,y)) boxes
  in
    if List.all (\box -> box.owner == "Player 1") boxesAroundCenter then
      td [] [text "1"]
    else
      td [] [text "2"]

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
