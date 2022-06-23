import Painter
    ( Point (MakePoint)
    , Color (MakeColor)
    , Pixel (MakePixel)
    , frame
    , paint
    )

import Shapes 
    ( Shape (MakeShape)
    , road
    , box
    )

-------------------------- QUESTIONS -------------------------------------------
-- how do you choose between let, where, guards, cases, ifs and matternmatching?
-- $ и . с чем едят
-- Куда класть определения типов с конструкторами. В отдельный модуль?
--

------------------------------- TODO ------------------------------------------
-- figure out the rules of indentation and multiline expressions
-- how to make exceptions for special cases? (pattern matching and guards will help)
-- optimize frame rendering with kewl alghorithms
--


----------------------------------- TYPES --------------------------------------
data Figure = MakeFigure Color Shape

--------------------------- FUNCS FOR MAIN -------------------------------------


----------------------------- MAIN ---------------------------------------------

figures :: [[Pixel]]
figures = [
    paint ';' (box (2, 2) (10, 5)),
    paint '=' (box (5, 4) (30, 4)),
    paint '!' (road (40, 4) (45, 4)),
    paint '%' (road (5, 0) (30, 0)),
    paint ')' (road (5, 1) (30, 1)),
    paint '(' (road (25, 2) (30, 2)),
    paint 'Ж' (road (5, 3) (30, 3))
    ]

renderFrame :: IO ()
renderFrame = putStr $ frame figures
