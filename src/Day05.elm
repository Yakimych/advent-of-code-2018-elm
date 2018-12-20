module Day05 exposing (main)

import Basics exposing (max)
import Browser
import Day05Input exposing (realInput, testInput, testInput2)
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
    ((Char.toUpper c1 == c2) || (Char.toUpper c2 == c1)) && c1 /= c2


reduceFunc : Char -> List Char -> List Char
reduceFunc c processedStr =
    case processedStr of
        [] ->
            [ c ]

        x :: xs ->
            if willDestruct x c then
                xs

            else
                c :: processedStr


processString : List Char -> List Char
processString chars =
    case chars of
        [] ->
            chars

        _ ->
            chars |> List.foldl reduceFunc [] |> List.reverse


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
        , div [] [ text model.inputString ]
        , div []
            [ text model.result
            ]
        , div []
            [ text ("Length: " ++ (model.result |> String.length |> String.fromInt)) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
