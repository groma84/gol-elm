module Main exposing (..)

import Array
import Html exposing (Html, text, div, button, h1)
import Html.Attributes exposing (type_, class, style)
import Html.Events exposing (onClick)
import Time exposing (Time, second)
import Types exposing (..)
import Gol exposing (..)


---- MODEL ----


type alias Model =
    { gameRunning : Bool
    , world : World
    }


init : ( Model, Cmd Msg )
init =
    -- TODO: Weltgröße und Start-Zellen anpassbar machen
    ( { gameRunning = False
      , world = createWorld 8 8 20
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | StartGame
    | StopGame
    | RestartGame
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartGame ->
            ( { model | gameRunning = True }, Cmd.none )

        StopGame ->
            ( { model | gameRunning = False }, Cmd.none )

        RestartGame ->
            -- TODO: Weltgröße und Start-Zellen anpassbar machen
            ( { model
                | gameRunning = False
                , world = createWorld 8 8 20
              }
            , Cmd.none
            )

        Tick time ->
            let
                newWorld =
                    iteration model.world
            in
                ( { model | world = newWorld }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        drawCell cell =
            case cell of
                Alive ->
                    div [ class "alive" ] [ text "X" ]

                Dead ->
                    div [ class "dead" ] [ text "O" ]

        drawCells cells =
            cells
                |> Array.map drawCell
                |> Array.toList

        cellSize =
            30
    in
        div []
            [ h1 [] [ text "Game of Life" ]
            , div
                [ class "world"
                , style [ ( "width", ((model.world.width * cellSize) |> toString) ++ "px" ), ( "height", ((model.world.height * cellSize) |> toString) ++ "px" ) ]
                ]
                (drawCells model.world.cells)
            , div []
                [ button [ type_ "button", onClick StartGame ] [ text "Start" ]
                , button [ type_ "button", onClick StopGame ] [ text "Stop" ]
                , button [ type_ "button", onClick RestartGame ] [ text "Restart" ]
                ]
            ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameRunning of
        True ->
            Time.every second Tick

        False ->
            Sub.none



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
