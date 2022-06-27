import Painter
    ( Point (MakePoint)
    , Color (MakeColor)
    , Pixel (MakePixel)
    , frame01
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
pt = MakePoint

ptt :: (Int, Int) -> Point
ptt = uncurry MakePoint

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
    building (pt 50 1) (pt 56 4) "бвкяж",
    building (pt 40 4) (pt 60 6) "TEST BUILdinG 68",
    building (pt 44 14) (pt 70 19) " в жопе города",
    building (pt 74 4) (pt 80 6) "БЮБЮБДБД",
    --building (pt 100 3) (pt 120 5) "Aboba",
    street (pt 150 3) (pt 120 3) "Tаrhun",
    street (pt 100 5) (pt 150 5) "Svobody",
    street (pt 120 0) (pt 120 5) "Vo",
    street (pt 150 2) (pt 399 2) "TUPIKOVYI TYPIK",
    street (pt 171 2) (pt 228 2) "автбобус",
    paint '7' $ box (2, 2) (10, 5),
    paint '5' $ box (5, 4) (30, 4),
    paint 'g' $ line (40, 4) (45, 4),
    paint 'o' $ line (5, 0) (30, 0),
    paint 'g' $ line (5, 1) (30, 1),
    paint 'У' $ line (25, 2) (30, 2),
    paint 'Ж' $ line (5, 3) (30, 3),
    paint 'H' $ routePoints $ zipWith pt [40, 44, 44] [4, 4, 14],
    paint '0' $ routePoints $ zipWith pt [136, 149, 149, 10] [2, 2, 6, 6],
    paint '4' $ routePoints $ zipWith pt [10, 10, 134, 134] [6, 4, 4, 1]
    ]
    

renderFrame :: IO ()
renderFrame = putStr $ frame01 figures
