module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)


---- MODEL ----

type alias Cell =
  { x : Int
  , y : Int
  , containsDot : Bool
  , canBeLine : Bool
  , isSelected : Bool
  }


type alias Model =
  { cell1 : Cell
  , cell2 : Cell
  , cell3 : Cell
  , cell4 : Cell
  , cell5 : Cell
  , cell6 : Cell
  , cell7 : Cell
  , cell8 : Cell
  , cell9 : Cell
  }


init : (Model, Cmd Msg)
init =
  (Model
    (Cell 1 1 True False False)
    (Cell 1 2 False True False)
    (Cell 1 3 True False False)
    (Cell 2 1 False True False)
    (Cell 2 2 False False False)
    (Cell 2 3 False True False)
    (Cell 3 1 True False False)
    (Cell 3 2 False True False)
    (Cell 3 3 True False False)
  , Cmd.none
  )



---- UPDATE ----


type Msg
  = SelectCell Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SelectCell cell ->
      let
        updatedCell = { cell | isSelected = True }
      in
        ({ model | cell1 = updatedCell }, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
  table []
    [
      tr []
        [ renderCell model.cell1
        , renderCell model.cell2
        , renderCell model.cell3
        ]
      , tr []
        [ renderCell model.cell4
        , renderCell model.cell5
        , renderCell model.cell6
        ]
      ,
      tr []
        [ renderCell model.cell7
        , renderCell model.cell8
        , renderCell model.cell9
        ]
    ]


renderCell : Cell -> Html Msg
renderCell cell =
  if cell.containsDot then
    td [ onClick (SelectCell cell) ] [ text "*" ]
  else
    td [] []


---- PROGRAM ----


main : Program Never Model Msg
main =
  Html.program
    { view = view
    , init = init
    , update = update
    , subscriptions = always Sub.none
    }
