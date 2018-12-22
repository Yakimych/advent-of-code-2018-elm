module Day08Logic exposing (calculateSum)

import Debug exposing (..)


calculateSum : List Int -> Int
calculateSum numbers =
    let
        nodes =
            numbers |> List.head |> Maybe.withDefault 0

        items =
            numbers |> List.drop 1 |> List.head |> Maybe.withDefault 0

        rest =
            numbers |> List.drop 2
    in
    case nodes of
        0 ->
            rest |> List.take items |> List.sum

        _ ->
            let
                ( sum, toClip ) =
                    getSumAndNumberOfElements nodes items rest

                newList =
                    nodes - 1 :: items :: (rest |> List.drop toClip)
            in
            sum + calculateSum newList


getSumAndNumberOfElements : Int -> Int -> List Int -> ( Int, Int )
getSumAndNumberOfElements nodes items elements =
    case nodes of
        0 ->
            ( elements |> List.take items |> List.sum, items )

        x ->
            let
                newNodes =
                    elements |> List.head |> Maybe.withDefault 0

                newItems =
                    elements |> List.drop 1 |> List.head |> Maybe.withDefault 0

                newRest =
                    elements |> List.drop 2

                ( sum, toClip ) =
                    getSumAndNumberOfElements newNodes newItems newRest

                ( latestSum, latestToClip ) =
                    getSumAndNumberOfElements (x - 1) items (elements |> List.drop (toClip + 2))
            in
            ( sum + latestSum, latestToClip + toClip + 2 )
