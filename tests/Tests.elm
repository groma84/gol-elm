module Tests exposing (..)

import Test exposing (..)
import Expect
import Gol exposing (..)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ describe "hand Gottes"
            [ test "Tot, 3 Nachbarn, Lebend" <|
                \_ ->
                    Expect.equal Alive (handGottes Dead 3)
            , test "Tot, 2 Nachbarn, Tot" <|
                \_ -> Expect.equal Dead (handGottes Dead 2)
            ]
        , describe "toCoords"
            [ test "0 bleibt 0" <|
                \_ ->
                    let
                        t =
                            ( 0, 0 )
                    in
                        Expect.equal t (toCoords 3 3 0)
            , test "erste Zeile, dritter Spalte" <|
                \_ ->
                    let
                        t =
                            ( 0, 2 )
                    in
                        Expect.equal t (toCoords 3 3 2)
            , test "zweite Zeile, dritter Spalte" <|
                \_ ->
                    let
                        t =
                            ( 1, 2 )
                    in
                        Expect.equal t (toCoords 3 3 5)
            ]
        ]
