import Canvas as Cnvs ( frame )
import Shapes as Shps ( road, box )

------------------------------- TODO ------------------------------------------
-- figure out the rules of indentation and multiline expressions
-- learn to make my own types and type constructors
-- how to make exceptions for special cases? (pattern matching and guards will help)
-- $ ?????


--------------------------- FUNCS FOR MAIN -------------------------------------
myBubbleSort :: [Int] -> [Int]
myBubbleSort xs = xs --IMPLEMENT!

-- sort by line. Then sort by x_pos in line and remove duplicates
arrangePoints :: [(Int, Int)] -> [(Int, Int)]
arrangePoints pointsToPaint = pointsToPaint --IMPLEMENT!


----------------------------- MAIN ---------------------------------------------
pointsToPaint :: [(Int, Int)]
pointsToPaint = arrangePoints $ box (2, 2) (10, 5) ++ road (5, 4) (30, 4) -- OH MY

renderFrame :: IO ()
renderFrame = putStr $ unlines $ frame pointsToPaint