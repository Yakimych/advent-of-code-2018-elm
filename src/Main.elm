module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Points exposing (Point, applyVelocity, testData)
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
    , currentIteration : Int
    , initialPoints : List Point
    , currentPoints : List Point
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { stepSizeString = "1"
      , stepSize = 1
      , currentIteration = 0
      , initialPoints = testData
      , currentPoints = testData
      }
    , Cmd.none
    )


type Msg
    = Next
    | Previous
    | ChangeStepSize String


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


view : Model -> Html Msg
view model =
    div []
        [ input [ value model.stepSizeString, onInput ChangeStepSize ] []
        , span [] [ text (String.fromInt model.stepSize) ]
        , div [] [ text ("Current iteration: " ++ String.fromInt model.currentIteration) ]
        , button [ onClick Previous ] [ text "Prev" ]
        , button [ onClick Next ] [ text "Next" ]
        , div [] [ text (model.currentPoints |> List.length |> String.fromInt) ]
        , svg [ Svg.Attributes.width "1200", Svg.Attributes.height "1200", viewBox "0 0 1200 1200" ]
            (model.currentPoints
                |> List.map
                    (\p ->
                        rect
                            [ x (p.x |> String.fromInt)
                            , y (p.y |> String.fromInt)
                            , Svg.Attributes.width "1"
                            , Svg.Attributes.height "1"
                            ]
                            []
                    )
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
