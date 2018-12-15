module Main exposing (main)

import Basics exposing (max)
import Browser
import Html exposing (Attribute, Html, button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Points exposing (Point, applyVelocity, firstMinIteration, realData, testData)
import Svg exposing (rect, svg)
import Svg.Attributes exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { stepSizeString : String
    , stepSize : Int
    , searchIterationsString : String
    , searchIterations : Int
    , currentIteration : Int
    , initialPoints : List Point
    , currentPoints : List Point
    , zoom : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialData =
            realData
    in
    ( { stepSizeString = "1"
      , stepSize = 1
      , searchIterationsString = "12000"
      , searchIterations = 12000
      , currentIteration = 0
      , initialPoints = initialData
      , currentPoints = initialData
      , zoom = 1
      }
    , Cmd.none
    )


type Msg
    = Next
    | Previous
    | ChangeStepSize String
    | ChangeSearchIterations String
    | Search
    | IncreaseZoom
    | DecreaseZoom


updateWithIteration : Model -> Int -> Model
updateWithIteration model iteration =
    { model
        | currentIteration = iteration
        , currentPoints = model.initialPoints |> List.map (\p -> applyVelocity p iteration)
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            let
                newIteration =
                    model.currentIteration + model.stepSize
            in
            ( updateWithIteration model newIteration, Cmd.none )

        Previous ->
            let
                newIteration =
                    model.currentIteration - model.stepSize
            in
            ( updateWithIteration model newIteration, Cmd.none )

        ChangeStepSize newStepSizeString ->
            let
                parsedStepSize =
                    String.toInt newStepSizeString
            in
            ( { model
                | stepSizeString = newStepSizeString
                , stepSize = String.toInt newStepSizeString |> Maybe.withDefault model.stepSize
              }
            , Cmd.none
            )

        ChangeSearchIterations newSearchIterationsString ->
            let
                parsedSearchedIterations =
                    String.toInt newSearchIterationsString
            in
            ( { model
                | searchIterationsString = newSearchIterationsString
                , searchIterations = String.toInt newSearchIterationsString |> Maybe.withDefault 12000
              }
            , Cmd.none
            )

        Search ->
            ( updateWithIteration model (firstMinIteration model.initialPoints), Cmd.none )

        IncreaseZoom ->
            if model.zoom >= 1 then
                ( { model | zoom = model.zoom + 1 }, Cmd.none )

            else if model.zoom >= 0.5 then
                ( { model | zoom = 1 }, Cmd.none )

            else
                ( { model | zoom = model.zoom * 2 }, Cmd.none )

        DecreaseZoom ->
            if model.zoom > 1 then
                ( { model | zoom = model.zoom - 1 }, Cmd.none )

            else
                ( { model | zoom = model.zoom / 2 }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ value model.searchIterationsString, onInput ChangeSearchIterations ] []
            , button [ onClick Search ] [ text "Search" ]
            ]
        , input [ value model.stepSizeString, onInput ChangeStepSize ] []
        , span [] [ text ("Step: " ++ String.fromInt model.stepSize) ]
        , div [] [ text ("Current iteration: " ++ String.fromInt model.currentIteration) ]
        , button [ onClick Previous ] [ text "Prev" ]
        , button [ onClick Next ] [ text "Next" ]
        , div [] [ text ("Current zoom level: " ++ String.fromFloat model.zoom) ]
        , button [ onClick DecreaseZoom ] [ text "-" ]
        , button [ onClick IncreaseZoom ] [ text "+" ]
        , div [] [ text ("Number of points: " ++ (model.currentPoints |> List.length |> String.fromInt)) ]
        , svg [ Svg.Attributes.width "1200", Svg.Attributes.height "1200", viewBox "0 0 1200 1200" ]
            (model.currentPoints
                |> List.map
                    (\p ->
                        rect
                            [ x ((toFloat p.x * model.zoom) |> String.fromFloat)
                            , y ((toFloat p.y * model.zoom) |> String.fromFloat)
                            , Svg.Attributes.width (Basics.max model.zoom 1 |> String.fromFloat)
                            , Svg.Attributes.height (Basics.max model.zoom 1 |> String.fromFloat)
                            ]
                            []
                    )
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
