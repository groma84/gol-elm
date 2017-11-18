module Gol exposing (..)


type Zelle
    = Dead
    | Alive


type alias Spielfeld =
    { zellen : List Zelle
    , breite : Breite
    , hoehe : Hoehe
    }


type alias AnzahlNachbarn =
    Int


type alias Breite =
    Int


type alias Hoehe =
    Int


toCoords : Breite -> Hoehe -> Int -> ( Int, Int )
toCoords breite hoehe index =
    let
        spaltenIndex =
            index % breite

        zeilenIndex =
            (index // hoehe)
    in
        ( zeilenIndex, spaltenIndex )


zaehleNachbarn : Spielfeld -> ( Int, Int ) -> Int
zaehleNachbarn spielfeld xy =
    0


handGottes : Zelle -> AnzahlNachbarn -> Zelle
handGottes zelle anzahlNachbarn =
    case zelle of
        Dead ->
            if (anzahlNachbarn == 3) then
                Alive
            else
                Dead

        Alive ->
            if (anzahlNachbarn == 2 || anzahlNachbarn == 3) then
                Alive
            else
                Dead


iteration : Spielfeld -> Spielfeld
iteration spielfeld =
    let
        gesamtvorgang index zelle =
            toCoords spielfeld.breite spielfeld.hoehe index
                |> zaehleNachbarn spielfeld
                |> handGottes zelle

        neueZellen =
            List.indexedMap gesamtvorgang spielfeld.zellen
    in
        { spielfeld | zellen = neueZellen }
