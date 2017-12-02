module Main exposing (..)

import Array
import Random
import Html exposing (Html, text, div, button, h1, form, input)
import Html.Attributes exposing (type_, class, classList, style, value)
import Html.Events exposing (onClick, onInput)
import Time exposing (Time, second)
import Types exposing (..)
import Gol exposing (..)


---- MODEL ----


type alias Model =
    { gameRunning : Bool
    , world : Maybe World
    , width : Int
    , height : Int
    , numberOfAliveCells : Int
    , worldNeedsRegeneration : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        initialWidth =
            8

        initialHeight =
            8

        randomCmd =
            createRandomValues initialWidth initialHeight
    in
        ( { gameRunning = False
          , world = Nothing
          , width = initialWidth
          , height = initialHeight
          , numberOfAliveCells = 20
          , worldNeedsRegeneration = False
          }
        , randomCmd
        )



---- UPDATE ----


type Msg
    = NoOp
    | StartGame
    | StopGame
    | RegenerateWorld
    | CreateNewWorld (List Int)
    | Tick Time
    | WidthChanged String
    | HeightChanged String
    | NumberOfAliveCellsChanged String


createRandomValues : Int -> Int -> Cmd Msg
createRandomValues width height =
    let
        totalFieldCount =
            width * height
    in
        Random.list totalFieldCount (Random.int 0 (totalFieldCount - 1))
            |> Random.generate CreateNewWorld


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartGame ->
            ( { model | gameRunning = True }, Cmd.none )

        StopGame ->
            ( { model | gameRunning = False }, Cmd.none )

        RegenerateWorld ->
            let
                generateNumbersCmd =
                    createRandomValues model.width model.height
            in
                ( { model | gameRunning = False, worldNeedsRegeneration = False }, generateNumbersCmd )

        CreateNewWorld randomNumbers ->
            let
                newWorld =
                    createWorld model.width model.height model.numberOfAliveCells randomNumbers
                        |> Just
            in
                ( { model | world = newWorld }, Cmd.none )

        Tick time ->
            let
                newWorld =
                    case model.world of
                        Nothing ->
                            model.world

                        Just oldWorld ->
                            iteration oldWorld |> Just
            in
                ( { model | world = newWorld }, Cmd.none )

        WidthChanged newVal ->
            let
                parsedNumber =
                    Result.withDefault model.width (String.toInt newVal)
            in
                ( { model | width = parsedNumber, worldNeedsRegeneration = True }, Cmd.none )

        HeightChanged newVal ->
            let
                parsedNumber =
                    Result.withDefault model.height (String.toInt newVal)
            in
                ( { model | height = parsedNumber, worldNeedsRegeneration = True }, Cmd.none )

        NumberOfAliveCellsChanged newVal ->
            let
                parsedNumber =
                    Result.withDefault model.numberOfAliveCells (String.toInt newVal)
            in
                ( { model | numberOfAliveCells = parsedNumber, worldNeedsRegeneration = True }, Cmd.none )



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

        drawCells world =
            case world of
                Nothing ->
                    [ div [] [] ]

                Just w ->
                    w.cells
                        |> Array.map drawCell
                        |> Array.toList

        cellSize =
            30

        startGameButton =
            case ( model.gameRunning, model.worldNeedsRegeneration ) of
                ( _, True ) ->
                    text ""

                ( True, _ ) ->
                    text ""

                ( False, _ ) ->
                    button [ type_ "button", onClick StartGame ] [ text "Start" ]

        stopGameButton =
            case model.gameRunning of
                True ->
                    button [ type_ "button", onClick StopGame ] [ text "Stop" ]

                False ->
                    text ""

        regenerateWorldButton =
            case model.gameRunning of
                True ->
                    text ""

                False ->
                    button [ type_ "button", onClick RegenerateWorld, classList [ ( "clickMe", model.worldNeedsRegeneration ) ] ] [ text "Generate New World" ]

        changeWorldSettingsForm =
            case model.gameRunning of
                True ->
                    text ""

                False ->
                    form []
                        [ input [ type_ "number", Html.Attributes.min "1", Html.Attributes.max "20", value (model.width |> toString), onInput WidthChanged ] []
                        , input [ type_ "number", Html.Attributes.min "1", Html.Attributes.max "20", value (model.height |> toString), onInput HeightChanged ] []
                        , input [ type_ "number", Html.Attributes.min "1", Html.Attributes.max "400", value (model.numberOfAliveCells |> toString), onInput NumberOfAliveCellsChanged ] []
                        ]

        world =
            case model.worldNeedsRegeneration of
                True ->
                    div [] []

                False ->
                    div
                        [ class "world"
                        , style [ ( "width", ((model.width * cellSize) |> toString) ++ "px" ), ( "height", ((model.height * cellSize) |> toString) ++ "px" ) ]
                        ]
                        (drawCells model.world)
    in
        div []
            [ h1 [] [ text "Game of Life" ]
            , div
                [ class "worldContainer"
                , classList [ ( "disabledWorldView", model.worldNeedsRegeneration ) ]
                , style [ ( "height", ((20 * cellSize) |> toString) ++ "px" ), ( "width", ((20 * cellSize) |> toString) ++ "px" ) ]
                ]
                [ world ]
            , div [ class "buttons" ]
                [ startGameButton
                , stopGameButton
                , regenerateWorldButton
                ]
            , div []
                [ changeWorldSettingsForm
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
