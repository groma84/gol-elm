module Tests exposing (..)

import Array
import Test exposing (..)
import Expect
import Gol exposing (..)


all : Test
all =
    describe "Game of Life Tests"
        [ describe "determineFate"
            [ test "Tot, 3 Nachbarn, Lebend" <|
                \_ ->
                    Expect.equal Alive (determineFate Dead 3)
            , test "Tot, 2 Nachbarn, Tot" <|
                \_ -> Expect.equal Dead (determineFate Dead 2)
            ]
        , describe "toCoords"
            [ test "0 bleibt 0" <|
                \_ ->
                    Expect.equal (Coords 0 0) (toCoords 3 3 0)
            , test "erste Zeile, dritter Spalte" <|
                \_ ->
                    Expect.equal (Coords 0 2) (toCoords 3 3 2)
            , test "zweite Zeile, dritter Spalte" <|
                \_ ->
                    Expect.equal (Coords 1 2) (toCoords 3 3 5)
            ]
        , describe "toIndex"
            [ test "0,0 wird 0" <|
                \_ ->
                    Expect.equal 0 (toIndex 3 (Coords 0 0))
            , test "0,1 wird 1" <|
                \_ ->
                    Expect.equal 1 (toIndex 3 (Coords 0 1))
            , test "1,0 wird 3" <|
                \_ ->
                    Expect.equal 3 (toIndex 3 (Coords 1 0))
            , test "1,1 wird 4" <|
                \_ ->
                    Expect.equal 4 (toIndex 3 (Coords 1 1))
            ]
        , describe "checkIfCoordsAreLegit"
            [ test "0,0 ist gueltig" <|
                \_ ->
                    Expect.true "Muss gueltig sein" (checkIfCoordsAreLegit (World Array.empty 3 3) (Coords 0 0))
            , test "2,2 ist gueltig" <|
                \_ ->
                    Expect.true "Muss gueltig sein" (checkIfCoordsAreLegit (World Array.empty 3 3) (Coords 2 2))
            , test "-1,0 ist ungueltig" <|
                \_ ->
                    Expect.false "Muss ungueltig sein" (checkIfCoordsAreLegit (World Array.empty 3 3) (Coords -1 0))
            , test "0,-1 ist ungueltig" <|
                \_ ->
                    Expect.false "Muss ungueltig sein" (checkIfCoordsAreLegit (World Array.empty 3 3) (Coords 0 -1))
            , test "3,0 ist ungueltig" <|
                \_ ->
                    Expect.false "Muss ungueltig sein" (checkIfCoordsAreLegit (World Array.empty 3 3) (Coords 3 0))
            , test "0,3 ist ungueltig" <|
                \_ ->
                    Expect.false "Muss ungueltig sein" (checkIfCoordsAreLegit (World Array.empty 3 3) (Coords 0 3))
            ]
        ]
