import Structs as Stru
import Painter as Pain

--TODO:
-- figure out the rules of indentation and multiline expressions
-- learn to make my own types and type constructors
-- how to make exceptions for special cases? (pattern matching and guards will help)
-- $ ?????

testCoordsToPaint :: [(Int, Int)]
testCoordsToPaint = [(0, 0), (3, 2), (5, 0)]
testMCScreen :: [(Int, Int, String)]
testMCScreen = Stru.mCScreen testCoordsToPaint

renderFrame :: IO ()
renderFrame = putStr $ unlines $ Pain.frame testMCScreen Stru.screenSize