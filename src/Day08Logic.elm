module Day08Logic exposing (calculateSum)


calculateSum : List Int -> Int
calculateSum numbers =
    getSumAndElementsUsed 1 0 numbers |> Tuple.first


getSumAndElementsUsed : Int -> Int -> List Int -> ( Int, Int )
getSumAndElementsUsed nodes items elements =
    case nodes of
        0 ->
            ( elements |> List.take items |> List.sum, items )

        x ->
            let
                newNodes =
                    elements |> List.head |> Maybe.withDefault 0

                newItems =
                    elements |> List.drop 1 |> List.head |> Maybe.withDefault 0

                newElements =
                    elements |> List.drop 2

                ( sum, toClip ) =
                    getSumAndElementsUsed newNodes newItems newElements

                ( latestSum, latestToClip ) =
                    getSumAndElementsUsed (x - 1) items (elements |> List.drop (toClip + 2))
            in
            ( sum + latestSum, latestToClip + toClip + 2 )
