module Gol exposing (..)

import Array exposing (..)
import Types exposing (..)


createWorld : Width -> Height -> NumberOfAliveCells -> List Int -> World
createWorld width height numberOfAliveCells mixNumbers =
    let
        totalFieldCount =
            width * height

        alivePart =
            let
                realNumberOfCells =
                    min totalFieldCount numberOfAliveCells
            in
                List.repeat realNumberOfCells Alive

        deadPart =
            List.repeat (totalFieldCount - numberOfAliveCells) Dead

        sortedCells =
            List.append alivePart deadPart

        randomizedCells =
            List.map2 (\a b -> ( a, b )) sortedCells mixNumbers
                |> List.sortBy (\( _, b ) -> b)
                |> List.map Tuple.first
                |> Array.fromList
    in
        { width = width
        , height = height
        , cells = randomizedCells
        }


toCoords : Width -> Height -> Index -> Coords
toCoords width height index =
    let
        columnIndex =
            index % width

        rowIndex =
            index // height
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
