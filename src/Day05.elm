module Day05 exposing (main)

import Basics exposing (max)
import Browser
import Day05Input exposing (realInput, testInput)
import Day05Logic exposing (findBestCaseLength, processString)
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
    { inputString : String
    , result1 : String
    , result2 : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialData =
            realInput
    in
    ( { inputString = initialData
      , result1 = ""
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
            let
                processedString =
                    processString (model.inputString |> String.toList) |> String.fromList
            in
            ( { model | result1 = processedString }, Cmd.none )

        CalculateResult2 ->
            ( { model | result2 = model.inputString |> String.toList |> findBestCaseLength }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick CalculateResult1 ] [ text "Part 1" ]
        , div [] [ text model.inputString ]
        , div []
            [ text model.result1
            ]
        , div []
            [ text ("Length: " ++ (model.result1 |> String.length |> String.fromInt)) ]
        , button [ onClick CalculateResult2 ] [ text "Part 2" ]
        , div [] [ text model.inputString ]
        , div []
            [ text (model.result2 |> String.fromInt)
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
