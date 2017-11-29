module Fixtures exposing (..)

import Array
import Types exposing (..)
import Gol exposing (..)


deadWorld : World
deadWorld =
    { width = 3
    , height = 3
    , cells = [ Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead ] |> Array.fromList
    }


aliveWorld : World
aliveWorld =
    { width = 3
    , height = 3
    , cells = [ Alive, Alive, Alive, Alive, Alive, Alive, Alive, Alive, Alive ] |> Array.fromList
    }
