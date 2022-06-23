import Painter
    ( Point (MakePoint)
    , Color (MakeColor)
    , Pixel (MakePixel)
    , frame
    , paint
    )

import Shapes 
    ( Shape (MakeShape)
    , line
    , box
    , building
    , street
    )

import Routes
    ( Route (MakeRoute)
    , routePoints
    )

-------------------------- QUESTIONS -------------------------------------------
-- how do you choose between let, where, guards, cases, ifs and matternmatching?
-- $ и . с чем едят
-- Куда класть определения типов с конструкторами. В отдельный модуль?
--

------------------------------- TODO ------------------------------------------
-- REFACTOR PAINTER IN A RECURSIVE MANNER. REDUCE RUNNING TIME
-- figure out the rules of indentation and multiline expressions
-- how to make exceptions for special cases? (pattern matching and guards will help)
-- optimize frame rendering with kewl alghorithms
-- handle more exceptions


----------------------------------- TYPES --------------------------------------
data Figure = MakeFigure Color Shape

--------------------------- FUNCS FOR MAIN -------------------------------------


----------------------------- MAIN ---------------------------------------------
--alias
pt :: Int -> Int -> Point
pt x y = MakePoint x y

ptt :: (Int, Int) -> Point
ptt (x, y) = MakePoint x y

testPoints :: [Point]
testPoints = map ptt [
    (136, 2),
    (149, 2),
    (149, 6),
    (10, 6),
    (10, 4),
    (134, 4),
    (134, 1) -- i=6
    ]

figures :: [[Pixel]]
figures = [ -- leading elements have higher priority in rendering
    building (pt 20 0) (pt 42 4) "TEST BUILDING 1",
    building (pt 30 1) (pt 50 4) "TEST BUILdinG 2",
    --building (pt 100 3) (pt 120 5) "Aboba",
    street (pt 150 3) (pt 120 3) "Trahova",
    street (pt 100 5) (pt 150 5) "Svobody",
    street (pt 120 0) (pt 120 5) "Vo",
    street (pt 150 2) (pt 399 2) "TUPIKOVYI TYPIK",
    paint ';' (box (2, 2) (10, 5)),
    paint '=' (box (5, 4) (30, 4)),
    paint '!' (line (40, 4) (45, 4)),
    paint '%' (line (5, 0) (30, 0)),
    paint ')' (line (5, 1) (30, 1)),
    paint '(' (line (25, 2) (30, 2)),
    paint 'Ж' (line (5, 3) (30, 3)),
    --routePoints '0' [
    paint '0' (routePoints [
        testPoints !! 0,
        testPoints !! 1,
        testPoints !! 2,
        testPoints !! 3
    ]),
    --routePoints '4' [ --ШЬЗДУЬУТЕ
    paint '4' (routePoints [
        testPoints !! 3,
        testPoints !! 4,
        testPoints !! 5,
        testPoints !! 6
    ])
    ]

renderFrame :: IO ()
renderFrame = putStr $ frame figures
