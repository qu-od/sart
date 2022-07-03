import Painter
    ( Point (MakePoint)
    , Color (MakeColor)
    , Pixel (MakePixel)
    , frame01
    , paint
    )

import Shapes 
    ( Figure (MakeFigure)
    , line        
    , box
    , building
    , street
    )

import Routes
    (-- Route (MakeRoute)
     route
    )

-------------------------- QUESTIONS -------------------------------------------
-- how do you choose between let, where, guards, cases, ifs and matternmatching?
-- $ и . с чем едят
-- Куда класть определения типов с конструкторами. В отдельный модуль?
--

------------------------------- TODO -------------------------------------------
-- Ultimate goal: automate routes making.
    -- Apply 2 points in the city and let the program to decide how to lay it out!
-- REFACTOR PAINTER IN A RECURSIVE MANNER. REDUCE RUNNING TIME
-- figure out the rules of indentation and multiline expressions
-- how to make exceptions for special cases? (pattern matching and guards will help)
-- optimize frame rendering with kewl alghorithms
-- handle more exceptions


----------------------------------- TYPES --------------------------------------


--------------------------- FUNCS FOR MAIN -------------------------------------


----------------------------- MAIN ---------------------------------------------
--alias
pt :: Int -> Int -> Point
pt = MakePoint

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

testFigures :: [Figure]
testFigures = [ -- leading elements have higher priority in rendering
-- все названия мелкий придумывал, так что не надо тут
    building (pt 20 0) (pt 42 4) "TEST BUILDING 1",
    building (pt 50 1) (pt 56 4) "5букв",
    building (pt 40 4) (pt 60 6) "TEST BUILdinG 68",
    building (pt 44 14) (pt 70 19) " в ебенях",
    building (pt 74 4) (pt 80 6) "что-то",
    -- 
    street (pt 150 3) (pt 120 3) "Gayorgyeva",
    street (pt 100 5) (pt 150 5) "Svobody",
    street (pt 120 0) (pt 120 5) "Vo",
    street (pt 150 2) (pt 399 2) "TUPIKOVYI TYPIK",
    street (pt 171 2) (pt 228 2) "Бельфегоровская",
    --
    route '1' (zipWith pt [40, 44, 44] [4, 4, 14]),
    route '2' (zipWith pt [136, 149, 149, 10] [2, 2, 6, 6]),
    route '3' (zipWith pt [10, 10, 134, 134] [6, 4, 4, 1]),
    --
    MakeFigure $ paint '7' $ box (2, 2) (10, 5),
    MakeFigure $ paint 'g' $ line (40, 4) (45, 4),
    MakeFigure $ paint 'o' $ line (5, 0) (30, 0)
    ]

pixels :: Figure -> [Pixel]
pixels (MakeFigure []) = [] 
pixels (MakeFigure [px]) = [px]
pixels (MakeFigure (px0:pxs)) = px0:pxs

figuresToPixels :: [Figure] -> [Pixel]
figuresToPixels = foldr (\fig acc -> pixels fig ++ acc) []

--------------------------------------- MAIN -----------------------------------
renderFrame :: IO ()
renderFrame = putStr $ frame01 $ figuresToPixels testFigures
