module Types exposing (..)

import Array exposing (..)


type Cell
    = Dead
    | Alive


type alias World =
    { cells : Array Cell
    , width : Width
    , height : Height
    }


type alias NumberOfNeighbours =
    Int


type alias Width =
    Int


type alias Height =
    Int


type alias Index =
    Int


type alias NumberOfAliveCells =
    Int


type alias Coords =
    { row : Int
    , column : Int
    }
