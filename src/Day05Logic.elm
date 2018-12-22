module Day05Logic exposing (findBestCaseLength, processString)


letters : List Char
letters =
    "abcdefghijklmnopqrstuvwxyz" |> String.toList


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
