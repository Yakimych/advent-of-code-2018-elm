module Day23Logic exposing (nanobotsInRange)

import Day23Input exposing (NanoInfo)


maxBy : (a -> Int) -> List a -> Maybe a
maxBy func list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just
                (List.foldl
                    (\p maxVal ->
                        if func p > func maxVal then
                            p

                        else
                            maxVal
                    )
                    x
                    xs
                )


nanobotsInRange : List NanoInfo -> Int
nanobotsInRange nanobots =
    1
