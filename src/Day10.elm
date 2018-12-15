module Day10 exposing (main)

import Basics exposing (max)
import Browser
import Day10Points exposing (Point, applyVelocity, firstMinIteration, realData, testData)
import Html exposing (Attribute, Html, button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
    , zoom : Float
    }


initialStepSize =
    1


initialSearchIterations =
    1200


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialData =
            realData
    in
    ( { stepSizeString = initialStepSize |> String.fromInt
      , stepSize = initialStepSize
      , searchIterationsString = initialSearchIterations |> String.fromInt
      , searchIterations = initialSearchIterations
      , currentIteration = 0
      , initialPoints = initialData
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            ( { model | currentIteration = model.currentIteration + model.stepSize }, Cmd.none )

        Previous ->
            ( { model | currentIteration = model.currentIteration - model.stepSize }, Cmd.none )

        ChangeStepSize newStepSizeString ->
            ( { model
                | stepSizeString = newStepSizeString
                , stepSize = String.toInt newStepSizeString |> Maybe.withDefault model.stepSize
              }
            , Cmd.none
            )

        ChangeSearchIterations newSearchIterationsString ->
            ( { model
                | searchIterationsString = newSearchIterationsString
                , searchIterations = String.toInt newSearchIterationsString |> Maybe.withDefault initialSearchIterations
              }
            , Cmd.none
            )

        Search ->
            ( { model | currentIteration = firstMinIteration model.initialPoints }, Cmd.none )

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
    let
        currentPoints =
            model.initialPoints |> List.map (\p -> applyVelocity p model.currentIteration)

        leftMostX =
            currentPoints
                |> List.map (\p -> p.x)
                |> List.minimum
                |> Maybe.withDefault 0

        topMostY =
            currentPoints
                |> List.map (\p -> p.y)
                |> List.minimum
                |> Maybe.withDefault 0
    in
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
        , div [] [ text ("Number of points: " ++ (currentPoints |> List.length |> String.fromInt)) ]
        , svg [ Svg.Attributes.width "1000", Svg.Attributes.height "1000", viewBox "0 0 1000 1000" ]
            (currentPoints
                |> List.map
                    (\p ->
                        rect
                            [ x ((toFloat (p.x - leftMostX) * model.zoom) |> String.fromFloat)
                            , y ((toFloat (p.y - topMostY) * model.zoom) |> String.fromFloat)
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
