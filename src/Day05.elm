module Day05 exposing (main)

import Basics exposing (max)
import Browser
import Day05Input exposing (realInput, testInput, testInput2)
import Html exposing (Attribute, Html, button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


letters : List Char
letters =
    "abcdefghijklmnopqrstuvwxyz" |> String.toList


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


withoutLetter : List Char -> Char -> List Char
withoutLetter chars c =
    chars |> List.filter (\char -> char /= c && char /= (c |> Char.toUpper))


findBestCaseLength : List Char -> Int
findBestCaseLength chars =
    let
        processedString =
            processString chars
    in
    letters
        |> List.map
            (\l -> withoutLetter chars l |> processString |> List.length)
        |> List.minimum
        |> Maybe.withDefault -1


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
