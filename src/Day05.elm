module Day05 exposing (main)

import Basics exposing (max)
import Browser
import Day05Input exposing (realInput, testInput)
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
    , result : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialData =
            realInput
    in
    ( { inputString = initialData
      , result = ""
      }
    , Cmd.none
    )


type Msg
    = CalculateResult


willDestruct : Char -> Char -> Bool
willDestruct c1 c2 =
    (Char.toUpper c1 == c2) || (Char.toUpper c2 == c1)


removePair : List Char -> List Char
removePair str =
    case str of
        [] ->
            str

        [ _ ] ->
            str

        [ _, _ ] ->
            str

        c1 :: c2 :: rest ->
            if willDestruct c1 c2 then
                rest

            else
                str


processString : List Char -> List Char
processString charList =
    case charList of
        [] ->
            charList

        [ _ ] ->
            charList

        c1 :: rest ->
            let
                clippedList =
                    removePair charList
            in
            if String.fromList charList == String.fromList clippedList then
                c1 :: processString rest

            else
                processString clippedList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CalculateResult ->
            let
                processedString =
                    processString (model.inputString |> String.toList) |> String.fromList
            in
            ( { model | result = processedString }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick CalculateResult ] [ text "Calculate" ]
        , div []
            [ text
                ("Length: "
                    ++ (model.result |> String.length |> String.fromInt)
                    ++ " string: "
                    ++ model.result
                )
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
