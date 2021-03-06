module Day10Points exposing (Point, applyVelocity, firstMinIteration, realData, testData)


type alias Point =
    { x : Int
    , y : Int
    , speedX : Int
    , speedY : Int
    }


applyVelocity : Point -> Int -> Point
applyVelocity point times =
    { point | x = point.x + point.speedX * times, y = point.y + point.speedY * times }


minMaxX : List Point -> ( Int, Int )
minMaxX points =
    List.foldl
        (\p ( min, max ) ->
            if p.x > max then
                ( min, p.x )

            else if p.x < min then
                ( p.x, max )

            else
                ( min, max )
        )
        ( 1000000, -1000000 )
        points


getSpreadX : List Point -> Int
getSpreadX points =
    let
        ( min, max ) =
            points |> minMaxX
    in
    max - min


firstMinIteration : List Point -> Int
firstMinIteration points =
    firstMinIterationRec points 1 (getSpreadX points)


firstMinIterationRec : List Point -> Int -> Int -> Int
firstMinIterationRec points iteration prevSpread =
    let
        spread =
            getSpreadX (points |> List.map (\p -> applyVelocity p iteration))
    in
    if spread > prevSpread then
        iteration - 1

    else
        firstMinIterationRec points (iteration + 1) spread


testData =
    [ { x = 9, y = 1, speedX = 0, speedY = 2 }
    , { x = 7, y = 0, speedX = -1, speedY = 0 }
    , { x = 3, y = -2, speedX = -1, speedY = 1 }
    , { x = 6, y = 10, speedX = -2, speedY = -1 }
    , { x = 2, y = -4, speedX = 2, speedY = 2 }
    , { x = -6, y = 10, speedX = 2, speedY = -2 }
    , { x = 1, y = 8, speedX = 1, speedY = -1 }
    , { x = 1, y = 7, speedX = 1, speedY = 0 }
    , { x = -3, y = 11, speedX = 1, speedY = -2 }
    , { x = 7, y = 6, speedX = -1, speedY = -1 }
    , { x = -2, y = 3, speedX = 1, speedY = 0 }
    , { x = -4, y = 3, speedX = 2, speedY = 0 }
    , { x = 10, y = -3, speedX = -1, speedY = 1 }
    , { x = 5, y = 11, speedX = 1, speedY = -2 }
    , { x = 4, y = 7, speedX = 0, speedY = -1 }
    , { x = 8, y = -2, speedX = 0, speedY = 1 }
    , { x = 15, y = 0, speedX = -2, speedY = 0 }
    , { x = 1, y = 6, speedX = 1, speedY = 0 }
    , { x = 8, y = 9, speedX = 0, speedY = -1 }
    , { x = 3, y = 3, speedX = -1, speedY = 1 }
    , { x = 0, y = 5, speedX = 0, speedY = -1 }
    , { x = -2, y = 2, speedX = 2, speedY = 0 }
    , { x = 5, y = -2, speedX = 1, speedY = 2 }
    , { x = 1, y = 4, speedX = 2, speedY = 1 }
    , { x = -2, y = 7, speedX = 2, speedY = -2 }
    , { x = 3, y = 6, speedX = -1, speedY = -1 }
    , { x = 5, y = 0, speedX = 1, speedY = 0 }
    , { x = -6, y = 0, speedX = 2, speedY = 0 }
    , { x = 5, y = 9, speedX = 1, speedY = -2 }
    , { x = 14, y = 7, speedX = -2, speedY = 0 }
    , { x = -3, y = 6, speedX = 2, speedY = -1 }
    ]


realData =
    [ { x = -42601, y = -53357, speedX = 4, speedY = 5 }
    , { x = 10946, y = 43042, speedX = -1, speedY = -4 }
    , { x = 21657, y = 32332, speedX = -2, speedY = -3 }
    , { x = -42617, y = 53749, speedX = 4, speedY = -5 }
    , { x = -21225, y = -53349, speedX = 2, speedY = 5 }
    , { x = 10929, y = -42647, speedX = -1, speedY = 4 }
    , { x = 53758, y = 43040, speedX = -5, speedY = -4 }
    , { x = 53745, y = 21622, speedX = -5, speedY = -2 }
    , { x = -42602, y = 32332, speedX = 4, speedY = -3 }
    , { x = 53741, y = -42644, speedX = -5, speedY = 4 }
    , { x = 10918, y = 21622, speedX = -1, speedY = -2 }
    , { x = 43075, y = -31929, speedX = -4, speedY = 3 }
    , { x = 32320, y = 21617, speedX = -3, speedY = -2 }
    , { x = -31914, y = 32330, speedX = 3, speedY = -3 }
    , { x = -53311, y = -42645, speedX = 5, speedY = 4 }
    , { x = -10491, y = 10911, speedX = 1, speedY = -1 }
    , { x = 21628, y = -21224, speedX = -2, speedY = 2 }
    , { x = -53355, y = -31932, speedX = 5, speedY = 3 }
    , { x = 21644, y = 32323, speedX = -2, speedY = -3 }
    , { x = 10925, y = -53353, speedX = -1, speedY = 5 }
    , { x = 43039, y = 32329, speedX = -4, speedY = -3 }
    , { x = -31919, y = -31933, speedX = 3, speedY = 3 }
    , { x = -10502, y = -10513, speedX = 1, speedY = 1 }
    , { x = 21659, y = -53353, speedX = -2, speedY = 5 }
    , { x = -42609, y = -21225, speedX = 4, speedY = 2 }
    , { x = -21181, y = 43041, speedX = 2, speedY = -4 }
    , { x = -21201, y = 21622, speedX = 2, speedY = -2 }
    , { x = -31911, y = -42638, speedX = 3, speedY = 4 }
    , { x = -31919, y = -31930, speedX = 3, speedY = 3 }
    , { x = -42596, y = -31937, speedX = 4, speedY = 3 }
    , { x = 53737, y = -42639, speedX = -5, speedY = 4 }
    , { x = 43072, y = -10509, speedX = -4, speedY = 1 }
    , { x = -10467, y = -31929, speedX = 1, speedY = 3 }
    , { x = 53789, y = -42646, speedX = -5, speedY = 4 }
    , { x = -53311, y = -42639, speedX = 5, speedY = 4 }
    , { x = 32373, y = -42643, speedX = -3, speedY = 4 }
    , { x = 21618, y = 43038, speedX = -2, speedY = -4 }
    , { x = -53342, y = 10905, speedX = 5, speedY = -1 }
    , { x = 10918, y = 43039, speedX = -1, speedY = -4 }
    , { x = -42621, y = 43034, speedX = 4, speedY = -4 }
    , { x = -42624, y = 43041, speedX = 4, speedY = -4 }
    , { x = -10487, y = 43039, speedX = 1, speedY = -4 }
    , { x = 53749, y = 43039, speedX = -5, speedY = -4 }
    , { x = 10916, y = 53743, speedX = -1, speedY = -5 }
    , { x = -10486, y = 43034, speedX = 1, speedY = -4 }
    , { x = 53761, y = -42644, speedX = -5, speedY = 4 }
    , { x = 32351, y = -10512, speedX = -3, speedY = 1 }
    , { x = -31916, y = 32332, speedX = 3, speedY = -3 }
    , { x = 53754, y = 43034, speedX = -5, speedY = -4 }
    , { x = 21631, y = -42638, speedX = -2, speedY = 4 }
    , { x = 32344, y = -10513, speedX = -3, speedY = 1 }
    , { x = 21655, y = 43041, speedX = -2, speedY = -4 }
    , { x = 10958, y = -31929, speedX = -1, speedY = 3 }
    , { x = 43061, y = 32327, speedX = -4, speedY = -3 }
    , { x = 32373, y = -31933, speedX = -3, speedY = 3 }
    , { x = -53305, y = -31928, speedX = 5, speedY = 3 }
    , { x = -53327, y = -42641, speedX = 5, speedY = 4 }
    , { x = -21221, y = -10513, speedX = 2, speedY = 1 }
    , { x = 32369, y = 53745, speedX = -3, speedY = -5 }
    , { x = -21204, y = -53352, speedX = 2, speedY = 5 }
    , { x = 10921, y = -42642, speedX = -1, speedY = 4 }
    , { x = -53307, y = -10512, speedX = 5, speedY = 1 }
    , { x = 53798, y = 10904, speedX = -5, speedY = -1 }
    , { x = -53336, y = 32327, speedX = 5, speedY = -3 }
    , { x = -21177, y = 21616, speedX = 2, speedY = -2 }
    , { x = 21623, y = 21616, speedX = -2, speedY = -2 }
    , { x = 32351, y = 10907, speedX = -3, speedY = -1 }
    , { x = -53350, y = 21621, speedX = 5, speedY = -2 }
    , { x = 32346, y = -21224, speedX = -3, speedY = 2 }
    , { x = 32335, y = 21613, speedX = -3, speedY = -2 }
    , { x = -42652, y = 21622, speedX = 4, speedY = -2 }
    , { x = 43035, y = 32327, speedX = -4, speedY = -3 }
    , { x = -10471, y = -31937, speedX = 1, speedY = 3 }
    , { x = 53772, y = 32327, speedX = -5, speedY = -3 }
    , { x = -21225, y = 32329, speedX = 2, speedY = -3 }
    , { x = 43051, y = 53747, speedX = -4, speedY = -5 }
    , { x = 10934, y = -31929, speedX = -1, speedY = 3 }
    , { x = -10467, y = 32330, speedX = 1, speedY = -3 }
    , { x = -10502, y = 10912, speedX = 1, speedY = -1 }
    , { x = 32341, y = 32332, speedX = -3, speedY = -3 }
    , { x = 10908, y = -53353, speedX = -1, speedY = 5 }
    , { x = 21623, y = -42644, speedX = -2, speedY = 4 }
    , { x = -10478, y = 32323, speedX = 1, speedY = -3 }
    , { x = 21620, y = -53355, speedX = -2, speedY = 5 }
    , { x = 21627, y = 21614, speedX = -2, speedY = -2 }
    , { x = -21192, y = 10906, speedX = 2, speedY = -1 }
    , { x = 21631, y = -53350, speedX = -2, speedY = 5 }
    , { x = 10930, y = -10514, speedX = -1, speedY = 1 }
    , { x = 53789, y = -42643, speedX = -5, speedY = 4 }
    , { x = 32335, y = 10908, speedX = -3, speedY = -1 }
    , { x = 10930, y = 32330, speedX = -1, speedY = -3 }
    , { x = 53748, y = -21227, speedX = -5, speedY = 2 }
    , { x = 43080, y = -10517, speedX = -4, speedY = 1 }
    , { x = 32358, y = 32329, speedX = -3, speedY = -3 }
    , { x = -42601, y = -10512, speedX = 4, speedY = 1 }
    , { x = 21666, y = 43033, speedX = -2, speedY = -4 }
    , { x = -10515, y = 53747, speedX = 1, speedY = -5 }
    , { x = -53352, y = 53743, speedX = 5, speedY = -5 }
    , { x = -42616, y = 53752, speedX = 4, speedY = -5 }
    , { x = -10513, y = -53353, speedX = 1, speedY = 5 }
    , { x = -10487, y = 32326, speedX = 1, speedY = -3 }
    , { x = 10954, y = 43042, speedX = -1, speedY = -4 }
    , { x = 53753, y = 43042, speedX = -5, speedY = -4 }
    , { x = -10487, y = 10906, speedX = 1, speedY = -1 }
    , { x = -10515, y = -53348, speedX = 1, speedY = 5 }
    , { x = 53761, y = -21225, speedX = -5, speedY = 2 }
    , { x = 10913, y = -42639, speedX = -1, speedY = 4 }
    , { x = -21217, y = -10511, speedX = 2, speedY = 1 }
    , { x = 21623, y = 21620, speedX = -2, speedY = -2 }
    , { x = 53763, y = -21227, speedX = -5, speedY = 2 }
    , { x = 43043, y = -21225, speedX = -4, speedY = 2 }
    , { x = 53746, y = -31933, speedX = -5, speedY = 3 }
    , { x = -31943, y = -31937, speedX = 3, speedY = 3 }
    , { x = 21608, y = 53743, speedX = -2, speedY = -5 }
    , { x = -53344, y = -10512, speedX = 5, speedY = 1 }
    , { x = 43048, y = -42641, speedX = -4, speedY = 4 }
    , { x = -21225, y = -53350, speedX = 2, speedY = 5 }
    , { x = 53758, y = -31933, speedX = -5, speedY = 3 }
    , { x = 43079, y = 32329, speedX = -4, speedY = -3 }
    , { x = 21636, y = -31931, speedX = -2, speedY = 3 }
    , { x = 21611, y = -31937, speedX = -2, speedY = 3 }
    , { x = -10471, y = -10511, speedX = 1, speedY = 1 }
    , { x = -42616, y = 53752, speedX = 4, speedY = -5 }
    , { x = 43036, y = 32323, speedX = -4, speedY = -3 }
    , { x = -10466, y = -31937, speedX = 1, speedY = 3 }
    , { x = -42609, y = -10510, speedX = 4, speedY = 1 }
    , { x = 53773, y = -21224, speedX = -5, speedY = 2 }
    , { x = -10507, y = -10515, speedX = 1, speedY = 1 }
    , { x = -42632, y = -31930, speedX = 4, speedY = 3 }
    , { x = -21190, y = 32328, speedX = 2, speedY = -3 }
    , { x = -53338, y = -42647, speedX = 5, speedY = 4 }
    , { x = 53741, y = 10906, speedX = -5, speedY = -1 }
    , { x = -53305, y = -10517, speedX = 5, speedY = 1 }
    , { x = -31885, y = -10517, speedX = 3, speedY = 1 }
    , { x = -10523, y = 53752, speedX = 1, speedY = -5 }
    , { x = 21611, y = 10906, speedX = -2, speedY = -1 }
    , { x = -31892, y = 32323, speedX = 3, speedY = -3 }
    , { x = 32318, y = 32332, speedX = -3, speedY = -3 }
    , { x = -42632, y = -53348, speedX = 4, speedY = 5 }
    , { x = 10918, y = 43037, speedX = -1, speedY = -4 }
    , { x = -31903, y = 53744, speedX = 3, speedY = -5 }
    , { x = -53318, y = 21622, speedX = 5, speedY = -2 }
    , { x = 43037, y = 32323, speedX = -4, speedY = -3 }
    , { x = -10499, y = -53356, speedX = 1, speedY = 5 }
    , { x = -10504, y = -31932, speedX = 1, speedY = 3 }
    , { x = 43075, y = 10910, speedX = -4, speedY = -1 }
    , { x = 43068, y = -10510, speedX = -4, speedY = 1 }
    , { x = -10499, y = -21220, speedX = 1, speedY = 2 }
    , { x = -42650, y = -10508, speedX = 4, speedY = 1 }
    , { x = -31892, y = -31928, speedX = 3, speedY = 3 }
    , { x = -21189, y = 43036, speedX = 2, speedY = -4 }
    , { x = 43076, y = 53752, speedX = -4, speedY = -5 }
    , { x = -42620, y = 21615, speedX = 4, speedY = -2 }
    , { x = -21177, y = 32330, speedX = 2, speedY = -3 }
    , { x = 10937, y = 43033, speedX = -1, speedY = -4 }
    , { x = -31898, y = 21614, speedX = 3, speedY = -2 }
    , { x = 53739, y = 10912, speedX = -5, speedY = -1 }
    , { x = -21206, y = -53353, speedX = 2, speedY = 5 }
    , { x = -21228, y = 21613, speedX = 2, speedY = -2 }
    , { x = 21609, y = -31932, speedX = -2, speedY = 3 }
    , { x = -42613, y = 21622, speedX = 4, speedY = -2 }
    , { x = -21177, y = -53351, speedX = 2, speedY = 5 }
    , { x = -21207, y = 21622, speedX = 2, speedY = -2 }
    , { x = 53761, y = -10508, speedX = -5, speedY = 1 }
    , { x = 53740, y = -31937, speedX = -5, speedY = 3 }
    , { x = 53756, y = -31937, speedX = -5, speedY = 3 }
    , { x = -21192, y = -21220, speedX = 2, speedY = 2 }
    , { x = -21174, y = 43033, speedX = 2, speedY = -4 }
    , { x = -10511, y = -53351, speedX = 1, speedY = 5 }
    , { x = 10900, y = -42647, speedX = -1, speedY = 4 }
    , { x = -53352, y = 21618, speedX = 5, speedY = -2 }
    , { x = 53798, y = 10904, speedX = -5, speedY = -1 }
    , { x = 43079, y = -53350, speedX = -4, speedY = 5 }
    , { x = -10498, y = -21218, speedX = 1, speedY = 2 }
    , { x = 10909, y = 10910, speedX = -1, speedY = -1 }
    , { x = -42613, y = 32331, speedX = 4, speedY = -3 }
    , { x = -42644, y = -21227, speedX = 4, speedY = 2 }
    , { x = -10510, y = -42645, speedX = 1, speedY = 4 }
    , { x = -53334, y = -21225, speedX = 5, speedY = 2 }
    , { x = 10918, y = 53745, speedX = -1, speedY = -5 }
    , { x = 21633, y = 53747, speedX = -2, speedY = -5 }
    , { x = -21205, y = -21223, speedX = 2, speedY = 2 }
    , { x = 21640, y = -10511, speedX = -2, speedY = 1 }
    , { x = -10503, y = 10908, speedX = 1, speedY = -1 }
    , { x = -53322, y = -10515, speedX = 5, speedY = 1 }
    , { x = 43076, y = 43042, speedX = -4, speedY = -4 }
    , { x = -53312, y = -53348, speedX = 5, speedY = 5 }
    , { x = 32333, y = 10911, speedX = -3, speedY = -1 }
    , { x = 53761, y = 32323, speedX = -5, speedY = -3 }
    , { x = -21173, y = 21622, speedX = 2, speedY = -2 }
    , { x = 10918, y = -21219, speedX = -1, speedY = 2 }
    , { x = 43069, y = -53352, speedX = -4, speedY = 5 }
    , { x = -42608, y = 21621, speedX = 4, speedY = -2 }
    , { x = -31938, y = 53752, speedX = 3, speedY = -5 }
    , { x = -42653, y = -21220, speedX = 4, speedY = 2 }
    , { x = 10905, y = -10511, speedX = -1, speedY = 1 }
    , { x = -21204, y = 43038, speedX = 2, speedY = -4 }
    , { x = 10925, y = -31937, speedX = -1, speedY = 3 }
    , { x = 53781, y = 53746, speedX = -5, speedY = -5 }
    , { x = -31906, y = -31937, speedX = 3, speedY = 3 }
    , { x = 10929, y = 10912, speedX = -1, speedY = -1 }
    , { x = -53346, y = 21618, speedX = 5, speedY = -2 }
    , { x = -31919, y = 21618, speedX = 3, speedY = -2 }
    , { x = 43083, y = -21225, speedX = -4, speedY = 2 }
    , { x = 43027, y = -42639, speedX = -4, speedY = 4 }
    , { x = -31916, y = 53752, speedX = 3, speedY = -5 }
    , { x = 32333, y = -10512, speedX = -3, speedY = 1 }
    , { x = -10487, y = 32330, speedX = 1, speedY = -3 }
    , { x = 43056, y = 53751, speedX = -4, speedY = -5 }
    , { x = -21189, y = -53351, speedX = 2, speedY = 5 }
    , { x = 32333, y = -31932, speedX = -3, speedY = 3 }
    , { x = 10905, y = -21227, speedX = -1, speedY = 2 }
    , { x = 53795, y = -31937, speedX = -5, speedY = 3 }
    , { x = -10490, y = 43040, speedX = 1, speedY = -4 }
    , { x = 21647, y = 21613, speedX = -2, speedY = -2 }
    , { x = 21616, y = 10907, speedX = -2, speedY = -1 }
    , { x = -10499, y = 10905, speedX = 1, speedY = -1 }
    , { x = -21232, y = 53749, speedX = 2, speedY = -5 }
    , { x = -42645, y = -31933, speedX = 4, speedY = 3 }
    , { x = -31927, y = 43037, speedX = 3, speedY = -4 }
    , { x = -31942, y = -31937, speedX = 3, speedY = 3 }
    , { x = 21620, y = -53348, speedX = -2, speedY = 5 }
    , { x = 43054, y = -42647, speedX = -4, speedY = 4 }
    , { x = 32329, y = -42640, speedX = -3, speedY = 4 }
    , { x = -10486, y = -21227, speedX = 1, speedY = 2 }
    , { x = 10929, y = -10509, speedX = -1, speedY = 1 }
    , { x = 43051, y = -31934, speedX = -4, speedY = 3 }
    , { x = 21666, y = -42638, speedX = -2, speedY = 4 }
    , { x = -21228, y = 32324, speedX = 2, speedY = -3 }
    , { x = -31899, y = 21615, speedX = 3, speedY = -2 }
    , { x = 32338, y = -42644, speedX = -3, speedY = 4 }
    , { x = -42624, y = -31931, speedX = 4, speedY = 3 }
    , { x = -53319, y = -21224, speedX = 5, speedY = 2 }
    , { x = -31911, y = 43033, speedX = 3, speedY = -4 }
    , { x = -42608, y = 53752, speedX = 4, speedY = -5 }
    , { x = 43064, y = -31929, speedX = -4, speedY = 3 }
    , { x = -53350, y = 32332, speedX = 5, speedY = -3 }
    , { x = -21212, y = 21618, speedX = 2, speedY = -2 }
    , { x = 21620, y = 43034, speedX = -2, speedY = -4 }
    , { x = -31887, y = -42645, speedX = 3, speedY = 4 }
    , { x = 53738, y = 10909, speedX = -5, speedY = -1 }
    , { x = 43067, y = 43042, speedX = -4, speedY = -4 }
    , { x = 21643, y = 43040, speedX = -2, speedY = -4 }
    , { x = -31924, y = 32323, speedX = 3, speedY = -3 }
    , { x = 10930, y = 53745, speedX = -1, speedY = -5 }
    , { x = 43027, y = 43042, speedX = -4, speedY = -4 }
    , { x = 10917, y = -21226, speedX = -1, speedY = 2 }
    , { x = -53363, y = 10912, speedX = 5, speedY = -1 }
    , { x = -10502, y = 32328, speedX = 1, speedY = -3 }
    , { x = 53749, y = -42647, speedX = -5, speedY = 4 }
    , { x = -42597, y = -31932, speedX = 4, speedY = 3 }
    , { x = -10522, y = 43039, speedX = 1, speedY = -4 }
    , { x = 10907, y = 10907, speedX = -1, speedY = -1 }
    , { x = 53755, y = 21618, speedX = -5, speedY = -2 }
    , { x = 10941, y = -53355, speedX = -1, speedY = 5 }
    , { x = 43087, y = 53743, speedX = -4, speedY = -5 }
    , { x = -53355, y = -31930, speedX = 5, speedY = 3 }
    , { x = 53740, y = -21227, speedX = -5, speedY = 2 }
    , { x = -42628, y = -21223, speedX = 4, speedY = 2 }
    , { x = 43029, y = -42647, speedX = -4, speedY = 4 }
    , { x = -10518, y = 32324, speedX = 1, speedY = -3 }
    , { x = -21204, y = 43038, speedX = 2, speedY = -4 }
    , { x = -31891, y = -53353, speedX = 3, speedY = 5 }
    , { x = 21636, y = 10904, speedX = -2, speedY = -1 }
    , { x = -21217, y = 10909, speedX = 2, speedY = -1 }
    , { x = -42651, y = -42647, speedX = 4, speedY = 4 }
    , { x = 10950, y = 32323, speedX = -1, speedY = -3 }
    , { x = -10512, y = -21227, speedX = 1, speedY = 2 }
    , { x = -31933, y = 43037, speedX = 3, speedY = -4 }
    , { x = -42624, y = -10515, speedX = 4, speedY = 1 }
    , { x = 10949, y = 10908, speedX = -1, speedY = -1 }
    , { x = 32325, y = 32329, speedX = -3, speedY = -3 }
    , { x = 10942, y = -21226, speedX = -1, speedY = 2 }
    , { x = 53798, y = 43034, speedX = -5, speedY = -4 }
    , { x = 10930, y = 32326, speedX = -1, speedY = -3 }
    , { x = 10898, y = 21622, speedX = -1, speedY = -2 }
    , { x = 43080, y = 10903, speedX = -4, speedY = -1 }
    , { x = 43027, y = -42639, speedX = -4, speedY = 4 }
    , { x = 10949, y = 43036, speedX = -1, speedY = -4 }
    , { x = 10926, y = -31936, speedX = -1, speedY = 3 }
    , { x = 21628, y = -21222, speedX = -2, speedY = 2 }
    , { x = 10949, y = -31934, speedX = -1, speedY = 3 }
    , { x = 10923, y = -53357, speedX = -1, speedY = 5 }
    , { x = -53346, y = 32324, speedX = 5, speedY = -3 }
    , { x = -21193, y = 21614, speedX = 2, speedY = -2 }
    , { x = -10467, y = 32326, speedX = 1, speedY = -3 }
    , { x = -53331, y = 43034, speedX = 5, speedY = -4 }
    , { x = 32325, y = -42646, speedX = -3, speedY = 4 }
    , { x = 21620, y = 53745, speedX = -2, speedY = -5 }
    , { x = 10902, y = -21225, speedX = -1, speedY = 2 }
    , { x = 43060, y = 10905, speedX = -4, speedY = -1 }
    , { x = -53339, y = -10516, speedX = 5, speedY = 1 }
    , { x = -42644, y = 10903, speedX = 4, speedY = -1 }
    , { x = 43069, y = -53353, speedX = -4, speedY = 5 }
    , { x = 21663, y = -42643, speedX = -2, speedY = 4 }
    , { x = 10949, y = 10908, speedX = -1, speedY = -1 }
    , { x = 53796, y = -21227, speedX = -5, speedY = 2 }
    , { x = -10515, y = -31934, speedX = 1, speedY = 3 }
    , { x = 10897, y = -31937, speedX = -1, speedY = 3 }
    , { x = 53745, y = 43034, speedX = -5, speedY = -4 }
    , { x = 10937, y = 43033, speedX = -1, speedY = -4 }
    , { x = -53319, y = 10910, speedX = 5, speedY = -1 }
    , { x = -53346, y = -53356, speedX = 5, speedY = 5 }
    , { x = -31938, y = 43033, speedX = 3, speedY = -4 }
    , { x = -31940, y = -31933, speedX = 3, speedY = 3 }
    , { x = 53779, y = -31933, speedX = -5, speedY = 3 }
    , { x = -42629, y = 10903, speedX = 4, speedY = -1 }
    , { x = -21205, y = -21218, speedX = 2, speedY = 2 }
    , { x = -31900, y = 32327, speedX = 3, speedY = -3 }
    , { x = 10900, y = -10508, speedX = -1, speedY = 1 }
    , { x = -42632, y = 43040, speedX = 4, speedY = -4 }
    , { x = -31933, y = 21613, speedX = 3, speedY = -2 }
    , { x = -10495, y = -31933, speedX = 1, speedY = 3 }
    , { x = -42627, y = 10903, speedX = 4, speedY = -1 }
    , { x = -42629, y = -21226, speedX = 4, speedY = 2 }
    , { x = -42626, y = -42638, speedX = 4, speedY = 4 }
    , { x = 10921, y = -21225, speedX = -1, speedY = 2 }
    , { x = 43039, y = 21620, speedX = -4, speedY = -2 }
    , { x = 21647, y = 43041, speedX = -2, speedY = -4 }
    , { x = 21626, y = 43038, speedX = -2, speedY = -4 }
    , { x = 43062, y = 32328, speedX = -4, speedY = -3 }
    , { x = -21229, y = -21218, speedX = 2, speedY = 2 }
    , { x = 43031, y = -53348, speedX = -4, speedY = 5 }
    , { x = 43060, y = 21620, speedX = -4, speedY = -2 }
    , { x = -53312, y = 32323, speedX = 5, speedY = -3 }
    , { x = -42626, y = -42647, speedX = 4, speedY = 4 }
    , { x = 43059, y = 32323, speedX = -4, speedY = -3 }
    , { x = -31940, y = -21218, speedX = 3, speedY = 2 }
    , { x = 53753, y = -10514, speedX = -5, speedY = 1 }
    , { x = 32349, y = -10516, speedX = -3, speedY = 1 }
    , { x = 32341, y = 32331, speedX = -3, speedY = -3 }
    , { x = 43048, y = -21219, speedX = -4, speedY = 2 }
    , { x = 10941, y = -10510, speedX = -1, speedY = 1 }
    , { x = 43035, y = 10903, speedX = -4, speedY = -1 }
    , { x = 10942, y = -21219, speedX = -1, speedY = 2 }
    , { x = 21652, y = -42638, speedX = -2, speedY = 4 }
    , { x = -53351, y = 21617, speedX = 5, speedY = -2 }
    , { x = -21213, y = 10904, speedX = 2, speedY = -1 }
    , { x = -53355, y = -21225, speedX = 5, speedY = 2 }
    , { x = -21193, y = -10508, speedX = 2, speedY = 1 }
    , { x = 43035, y = 53743, speedX = -4, speedY = -5 }
    , { x = -42628, y = 53747, speedX = 4, speedY = -5 }
    , { x = 53750, y = -31934, speedX = -5, speedY = 3 }
    , { x = 53778, y = 53750, speedX = -5, speedY = -5 }
    , { x = -53338, y = 32323, speedX = 5, speedY = -3 }
    , { x = -53358, y = -31937, speedX = 5, speedY = 3 }
    , { x = 21663, y = -31936, speedX = -2, speedY = 3 }
    , { x = -21209, y = 43037, speedX = 2, speedY = -4 }
    , { x = -10467, y = 32328, speedX = 1, speedY = -3 }
    , { x = -21173, y = 21613, speedX = 2, speedY = -2 }
    , { x = 10899, y = 10908, speedX = -1, speedY = -1 }
    , { x = -31935, y = -31932, speedX = 3, speedY = 3 }
    , { x = 21643, y = -42640, speedX = -2, speedY = 4 }
    , { x = 21651, y = 43039, speedX = -2, speedY = -4 }
    , { x = -42628, y = 21622, speedX = 4, speedY = -2 }
    , { x = 10926, y = -10515, speedX = -1, speedY = 1 }
    , { x = 32335, y = 43038, speedX = -3, speedY = -4 }
    , { x = 53773, y = 10905, speedX = -5, speedY = -1 }
    , { x = -31939, y = -53357, speedX = 3, speedY = 5 }
    , { x = -42643, y = 53743, speedX = 4, speedY = -5 }
    , { x = -21199, y = 21617, speedX = 2, speedY = -2 }
    , { x = -10499, y = -31931, speedX = 1, speedY = 3 }
    ]
