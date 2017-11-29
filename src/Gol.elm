module Gol exposing (..)

import Array exposing (..)
import Types exposing (..)


createWorld : Width -> Height -> NumberOfAliveCells -> World
createWorld width height numberOfAliveCells =
    let
        -- TODO: Begrenzung von numberOfAliveCells auf width * height
        length =
            width * height

        alivePart =
            List.repeat numberOfAliveCells Alive

        deadPart =
            List.repeat (length - numberOfAliveCells) Dead
    in
        -- TODO: Mischen der Positionen der lebenden Zellen
        { width = width
        , height = height
        , cells = List.append alivePart deadPart |> Array.fromList
        }


toCoords : Width -> Height -> Index -> Coords
toCoords width height index =
    let
        columnIndex =
            index % width

        rowIndex =
            (index // height)
    in
        Coords rowIndex columnIndex


toIndex : Width -> Coords -> Index
toIndex width coords =
    coords.row * width + coords.column


checkIfCoordsAreLegit : World -> Coords -> Bool
checkIfCoordsAreLegit world coords =
    if (coords.row < 0 || coords.column < 0 || coords.row >= world.width || coords.column >= world.height) then
        False
    else
        True


countAliveNeighbours : World -> Coords -> Int
countAliveNeighbours world coords =
    let
        topLeft =
            Coords (coords.row - 1) (coords.column - 1)

        topMiddle =
            Coords (coords.row - 1) (coords.column)

        topRight =
            Coords (coords.row - 1) (coords.column + 1)

        left =
            Coords (coords.row) (coords.column - 1)

        right =
            Coords (coords.row) (coords.column + 1)

        bottomLeft =
            Coords (coords.row + 1) (coords.column - 1)

        bottomMiddle =
            Coords (coords.row + 1) (coords.column)

        bottomRight =
            Coords (coords.row + 1) (coords.column + 1)

        neighbourCoords =
            [ topLeft, topMiddle, topRight, left, right, bottomLeft, bottomMiddle, bottomRight ]

        legitCoords =
            List.filter (checkIfCoordsAreLegit world) neighbourCoords
    in
        legitCoords
            |> List.map (toIndex world.width)
            |> List.map (\i -> Array.get i world.cells)
            |> List.map (\c -> Maybe.withDefault Dead c)
            |> List.filter
                (\c ->
                    case c of
                        Alive ->
                            True

                        Dead ->
                            False
                )
            |> List.length


determineFate : Cell -> NumberOfNeighbours -> Cell
determineFate cell numberOfNeighbours =
    case cell of
        Dead ->
            if (numberOfNeighbours == 3) then
                Alive
            else
                Dead

        Alive ->
            if (numberOfNeighbours == 2 || numberOfNeighbours == 3) then
                Alive
            else
                Dead


iteration : World -> World
iteration world =
    let
        oneRound index cell =
            toCoords world.width world.height index
                |> countAliveNeighbours world
                |> determineFate cell

        newCells =
            Array.indexedMap oneRound world.cells
    in
        { world | cells = newCells }
