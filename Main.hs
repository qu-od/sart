import Painter as Pntr ( frame, paint )
import Shapes as Shps ( road, box )

-------------------------- QUESTIONS -------------------------------------------
-- how do you choose between let, where, guards, cases, ifs and matternmatching?

------------------------------- TODO ------------------------------------------
-- figure out the rules of indentation and multiline expressions
-- learn to make my own types and type constructors
-- how to make exceptions for special cases? (pattern matching and guards will help)
-- $ ?????


--------------------------- FUNCS FOR MAIN -------------------------------------


----------------------------- MAIN ---------------------------------------------
figures :: [[(Int, Int, String)]]
figures = [
    paint '#' (box (2, 2) (10, 5)),
    paint '=' (box (5, 4) (30, 4)),
    paint '!' (road (40, 4) (45, 4)),
    paint '%' (road (5, 0) (30, 0)),
    paint ')' (road (5, 1) (30, 1)),
    paint '(' (road (25, 2) (30, 2)),
    paint 'Ж' (road (5, 3) (30, 3))
    ]

renderFrame :: IO ()
renderFrame = putStr $ frame figures
