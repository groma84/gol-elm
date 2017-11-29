module Main exposing (..)

import Html exposing (Html, text, div, button)
import Html.Attributes exposing (type_)
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
    div []
        [ button [ type_ "button", onClick StartGame ] [ text "Start" ]
        , button [ type_ "button", onClick StopGame ] [ text "Stop" ]
        , button [ type_ "button", onClick RestartGame ] [ text "Restart" ]
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
