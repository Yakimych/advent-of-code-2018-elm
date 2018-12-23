module Day23 exposing (main)

import Browser
import Day23Input exposing (NanoInfo, realInput, testInput)
import Day23Logic exposing (nanobotsInRange)
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
    { inputList : List NanoInfo
    , result1 : Int
    , result2 : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { inputList = realInput
      , result1 = -1
      , result2 = -1
      }
    , Cmd.none
    )


type Msg
    = CalculateResult1
    | CalculateResult2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CalculateResult1 ->
            ( { model | result1 = nanobotsInRange model.inputList }, Cmd.none )

        CalculateResult2 ->
            ( { model | result2 = nanobotsInRange model.inputList }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick CalculateResult1 ] [ text "Part 1" ]
        , div []
            [ text (model.result1 |> String.fromInt)
            ]
        , button [ onClick CalculateResult2 ] [ text "Part 2" ]
        , div []
            [ text (model.result2 |> String.fromInt)
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
