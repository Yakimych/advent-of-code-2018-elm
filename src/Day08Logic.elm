module Day08Logic exposing (calculateSum)


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
                    nodes - 1 :: items :: rest |> List.drop toClip
            in
            sum + calculateSum newList


getSumAndNumberOfElements : Int -> Int -> List Int -> ( Int, Int )
getSumAndNumberOfElements nodes items elements =
    case nodes of
        0 ->
            ( items, elements |> List.take items |> List.sum )

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
            in
            getSumAndNumberOfElements (x - 1) items (elements |> List.drop toClip)



-- 0
-- 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
-- 33
-- 1 3 1 1 0 1 99 2 1 1 2
-- 33 + 99
-- 1 3 0 1 2 1 1 2
-- 33 + 99 + 2
-- 0 3 1 1 2
-- 33 + 99 + 2 + (1 + 1 + 2) = 100 + 33 + 2 + 1 + 2 = 133 + 5 = 138
