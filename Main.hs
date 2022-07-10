{-# LANGUAGE ViewPatterns #-}

import Painter
    ( IntPoint (MkIntPoint, iX, iY)
    , GenColor (MkGenColor)
    , GenPixel (MkGenPixel)
    , Frame
    , Direction (NoDirection, Up, Down, Left', Right') --deprecated
    , Shape (EmptyShape, Building, StreetPD, StreetPP, Route)
    -- StreetPD deprecated after 0.1.2
    , Busstop (Intersection, Deadend, Extra) -- for advanced routes
    , ensureStreetPP
    , streetPDP2
    , streetPPLen
    , streetAxis
    , interpolate'
    , isStreet
    , streetRequiredError
    , frame012
    , paint -- deprecated
    )

import Shapes 
    ( testShapes
    , testBusstops
    , testMiscPixels
    )

import Control.Monad 
    ( when
    , sequence
    , forM
    , mapM
    , forever
    )

import Data.Char (digitToInt)

--import GHC.Stack (HasCallStack)


-------------------------- QUESTIONS -------------------------------------------
-- may I carry state as an argument in the reqursive main?
-- -- or it'd be better to save state in a file and load it everytime?
-- -- -- I think I should try to carry state without file-jerk on every frame somehow


----------------------------- TODO ---------------------------------------------
-- state of frame and a cursor saved in a file (0.1.3)
-- user cursor 
    -- move it with WASD input
    -- action button (to build shapes in-console)
    -- feedback for actions (0.1.4)
-- render names of the 
    -- streets (0.1.5)
    -- buuldings 
    -- routes (0.1.6)
-- automatic city generation (0.2)
-- advanced routes
    -- render in parallel to the streets and to each other (0.2.1)
    -- bind them to street grid (0.2.2)
    -- autogenerate route from point A to point B (0.2.3)
-- animate (0.3....)
    -- busses traffic
    -- other traffic
    -- responsive traffic (traffic jams must occur)


-------------------------- pure functions for main -----------------------------
startingPoint :: IntPoint
startingPoint = MkIntPoint 60 37

updatePoint :: IntPoint -> Char -> IntPoint
updatePoint pt char
    | char ==  'a' = MkIntPoint (iX pt - 1) (iY pt)
    | char ==  'd' = MkIntPoint (iX pt + 1) (iY pt)
    | char ==  'w' = MkIntPoint (iX pt) (iY pt - 1)
    | char ==  's' = MkIntPoint (iX pt) (iY pt + 1)
    | otherwise = error $ "Wrong char for the movement direction." ++ [char]

renderFrame :: String
renderFrame = frame012 (testShapes, testBusstops)

renderFrameWithUserBusstop :: IntPoint -> String
renderFrameWithUserBusstop intPoint =
    frame012 (testShapes, testBusstops ++ [Extra EmptyShape intPoint])


------------------------- impure functions for main ----------------------------
putBusstops :: IO ()
putBusstops = forever $ do
    putStr "type X value: "
    x <- getLine
    putStr "type Y value: "
    y <- getLine
    putStrLn $ "x=" ++ x ++ ", y=" ++ y
    putStrLn $ renderFrameWithUserBusstop $ MkIntPoint (read x :: Int) (read y :: Int)
    return ()


--------------------------------- MAIN -----------------------------------------
main :: IO ()
main = forever $ do
    movementString <- getLine
    let newPoint = updatePoint startingPoint (head movementString) --head is unsafe
        -- save point state in a FILE tonight!! And also a frame state
    putStrLn $ renderFrameWithUserBusstop newPoint
