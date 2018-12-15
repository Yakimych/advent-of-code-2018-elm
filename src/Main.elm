module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { stepSizeString = "1", stepSize = 1, currentIteration = 0 }, Cmd.none )


type Msg
    = Next
    | Previous
    | ChangeStepSize String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            ( { model | currentIteration = model.currentIteration + model.stepSize }, Cmd.none )

        Previous ->
            ( { model | currentIteration = model.currentIteration - model.stepSize }, Cmd.none )

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
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
