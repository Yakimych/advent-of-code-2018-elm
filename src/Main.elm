module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { stepSizeString : String
    , currentIteration : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { stepSizeString = "1", currentIteration = 0 }, Cmd.none )


type Msg
    = Next
    | Previous
    | ChangeStepSize String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            ( { model | currentIteration = model.currentIteration + 1 }, Cmd.none )

        Previous ->
            ( { model | currentIteration = model.currentIteration - 1 }, Cmd.none )

        ChangeStepSize newStepSizeString ->
            ( { model | stepSizeString = newStepSizeString }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ input [ value model.stepSizeString, onInput ChangeStepSize ] []
        , div [] [ text "Test" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
