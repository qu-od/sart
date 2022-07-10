{-# LANGUAGE ViewPatterns #-}

import Painter
    ( IntPoint (MkIntPoint, iX, iY)
    , GenColor (MkGenColor)
    , GenPixel (MkGenPixel)
    , Frame
    , Shape (EmptyShape, Building, StreetPP, Route)
    , Busstop (Intersection, Deadend, Extra)
    , FrameContents
    , ensureStreetPP
    , streetPDP2
    , streetPPLen
    , streetAxis
    , interpolate'
    , isStreet
    , streetRequiredError
    , frame012
    , palette
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

------------------------------- TYPES ------------------------------------------
type GameState = String

type FrameContents = ([Shape], [Busstop], [GenPixel Char])


-------------------------- pure functions for main -----------------------------
gameStateFileName :: String
gameStateFileName = "game state.txt"

startingPoint :: IntPoint
startingPoint = MkIntPoint 60 37

updatePoint :: IntPoint -> Char -> IntPoint
updatePoint pt char
    | char ==  'a' = MkIntPoint (iX pt - 1) (iY pt)
    | char ==  'd' = MkIntPoint (iX pt + 1) (iY pt)
    | char ==  'w' = MkIntPoint (iX pt) (iY pt - 1)
    | char ==  's' = MkIntPoint (iX pt) (iY pt + 1)
    | otherwise = error $ "Wrong char for the movement direction." ++ [char]

formFrameWithUserBusstop :: IntPoint -> String
formFrameWithUserBusstop intPoint =
    frame012 (testShapes, testBusstops ++ [Extra EmptyShape intPoint])

formFrame :: String
formFrame = frame012 (testShapes, testBusstops, [])

-- |just a GameState parser
getFrameContents :: GameState -> FrameContents
getFrameContents = undefined

updateGameState :: GameState -> String -> GameState
updateGameState prevGameState userActionString = undefined


------------------------- impure functions for main ----------------------------
putBusstops :: IO ()
putBusstops = forever $ do
    putStr "type X value: "
    x <- getLine
    putStr "type Y value: "
    y <- getLine
    putStrLn $ "x=" ++ x ++ ", y=" ++ y
    putStrLn $ formFrameWithUserBusstop (
        MkIntPoint (read x :: Int) (read y :: Int)
        )
    return ()

-- TEST THIS GOOD PRACTICE
--usingTempFile :: IO ()
--usingTempFile = undefined

saveGameIntoAFile :: GameState -> IO ()
saveGameIntoAFile = writeFile gameStateFileName --part. app.

uglyLoadGameFromAFile :: IO GameState
uglyLoadGameFromAFile = do
    handle <- openFile gameStateFileName readMode 
    contents <- hGetContents handle --unwrap IO String into String
    hClose handle
    return contents --wrap String to IO String

loadGameFromAFile :: IO String
loadGameFromAFile = readFile gameStateFileName
    
renderFrame :: FrameContents -> IO ()
renderFrame fContents = putStrLn $ frame012 fContents

-- |Load GameState (GameState is just a String)
-- |Render Frame with FrameContents encoded in a GameState
-- |get user input
-- |update GameState accordingly to user input
-- |save new gameState
singleStageGame :: IO ()
singleStageGame = do
    gameState <- loadGameFromAFile
    () <- renderFrame $ getFrameContents gameState
    userActionLine <- getLine
    let newGameState = processUserAction userAction gameState
    () <- saveGameIntoAFile newGameState

-- |if user input is not " ", calls itself to do next stage of the game
-- |and when user input is " ", returns the last GameState
gameStageWithoutJerkingAFile :: GameState -> IO GameState
gameStageWithoutJerkingAFile gameState = do
    () <- renderFrame $ getFrameContents gameState
    userActionLine <- getLine
    if userActionLine == " "
        then do
            let newGameState = updateGameState userAction gameState 
            gameStageWithoutJerkingAFile newGameState
    else return gameState

infiniteStagesGame :: IO ()
infiniteStagesGame = do
    initialGameState <- loadGameFromAFile
    lastGameState <- gameStageWithoutJerkingAFile initialGameState --looped reqursively
    -- when " " unput occured, reqursion ended and we're making an autosave
    saveGameIntoAFile lastGameState


--------------------------------- MAIN -----------------------------------------
main :: IO ()
main = forever $ do
    movementString <- getLine
    let newPoint = updatePoint startingPoint (head movementString) --head is unsafe
        -- save point state in a FILE tonight!! And also a frame state
    putStrLn $ formFrameWithUserBusstop newPoint
