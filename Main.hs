{-# LANGUAGE ViewPatterns #-}

import Painter
    ( Entry (entry)
    , IntPoint (MkIntPoint, iX, iY)
    , GenColor (MkGenColor)
    , GenPixel (MkGenPixel)
    , Frame
    , Shape (EmptyShape, Building, StreetPP, Route)
    , Busstop (Intersection, Deadend, Extra)
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

import System.IO (openFile, hGetContents, hClose, IOMode(ReadMode))

--import GHC.Stack (HasCallStack)

------------------------------ META --------------------------------------------
-- DON'T YOU DARE TO OVEROPTIMIZE! IT WORKS? IF YES - MOVE ON.
-- SART WILL BE DONE IN THIS WEEK NO MATTER HOW FAR WE'LL GO 

-------------------------- QUESTIONS -------------------------------------------
-- may I carry state as an argument in the reqursive main?
-- -- or it'd be better to save state in a file and load it everytime?
-- -- -- I think I should try to carry state without file-jerk on every frame somehow


----------------------------- TODO ---------------------------------------------
-- build busstops inside a frame012!
    -- (busstops are fully defined by streets after all)
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
type GameMeta = (CursorPoint, BufferedPoints, Stage)

type CursorPoint = IntPoint
type BufferedPoints = [IntPoint]
type Stage = Int



------------------------------ test functions ----------------------------------
--formFrameWithUserBusstop :: IntPoint -> String
--formFrameWithUserBusstop intPoint =
    --frame012 (testShapes, testBusstops ++ [Extra EmptyShape intPoint], [])

--formFrame :: String
--formFrame = frame012 (testShapes, testBusstops, [])

--uglyLoadGameFromAFile :: IO GameState --DEPRECATED
--uglyLoadGameFromAFile = do
    --handle <- openFile gameStateFileName ReadMode 
    --contents <- hGetContents handle --unwrap IO String into String
    --hClose handle
    --return contents --wrap String to IO String

--main = forever $ do
    --movementString <- getLine
    --let newPoint = updatePoint startingPoint (head movementString) --head is unsafe
        -- save point state in a FILE tonight!! And also a frame state
    --putStrLn $ formFrameWithUserBusstop newPoint

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

testFrameContents :: FrameContents
testFrameContents = (
    testShapes
    , testBusstops
    , [MkGenPixel (MkIntPoint 0 0) (MkGenColor '5')]
    )


testGameState :: FrameContents -> GameMeta -> GameState
testGameState (shapes, busstops, pixels) = concat [
    -- unlines is excessive cuz entries
    -- are already multiline and also with '\n' at head
    map entry shapes,
    map entry busstops,
    map entry pixels,
    "\n--- CURSOR ---\n", entry startingPoint,
    "\n--- BUFFERED POINTS ---\n", entry [],
    "\n--- STAGE ---\n", entry (0 :: Int)
    ]

-- |just a GameState PARSER
-- |String in a returned pair contains other game info other than shape
-- |function will be updated for sure
parseGameState :: GameState -> (FrameContents, GameMeta)
parseGameState gameState = undefined

getFrameContents :: GameState -> FrameContents
getFrameContents gameState = 
    (\(frameContents, _) -> frameContents) $ parseGameState gameState

-- |How the game state could be updated? (" " just quits the game completely)
-- |1. cursor moved with 'w' 'a' 's' 'd' inputs (cursor coords updated)
-- |2. building point created with 'b' (p1 - buffered, p2 - built)
-- |3. street point created with 't' (p1 - buffered, p2 - built)
-- |4. route point created with 'r' (points buffered one after another)
-- |5. route last point created with 'l' (route built)
-- -- |if inputs are (not bb) or (not tt) or (not r...rl) just flush the buffer
updateGameState :: GameState -> String -> GameState
updateGameState prevGameState inputLine = case inputLine of 
    "w" -> undefined
    "a" -> undefined
    "s" -> undefined
    "d" -> undefined
    "b" -> undefined
    "t" -> undefined
    "r" -> undefined
    "l" -> undefined
    _ -> error "Unexpected input line"
    where 
        i :: String
        i = inputLine
        gameStateLines :: [String]
        gameStateLines = lines prevGameState
        cursorLine :: String
        cursorLine = 


------------------------- impure functions for main ----------------------------
makeFileWithInitialGameState :: IO ()
makeFileWithInitialGameState = do 
    saveGameIntoAFile testGameState

-- TEST THIS GOOD PRACTICE
--usingTempFile :: IO ()
--usingTempFile = 

saveGameIntoAFile :: GameState -> IO ()
saveGameIntoAFile = writeFile gameStateFileName --part. app.

loadGameFromAFile :: IO GameState
loadGameFromAFile = readFile gameStateFileName
    
renderFrame :: FrameContents -> IO ()
renderFrame fContents = putStrLn $ frame012 fContents

-- |Load GameState (GameState is just a String)
-- |Render Frame with FrameContents encoded in a GameState
-- |get user input
-- |update GameState accordingly to user input
-- |save new gameState
singleStageGame :: IO () -- DEPRECATED
singleStageGame = do
    gameState <- loadGameFromAFile
    () <- renderFrame $ getFrameContents gameState
    userActionLine <- getLine
    let newGameState = updateGameState userActionLine gameState
    saveGameIntoAFile newGameState

-- |if user input is not " ", calls itself to do next stage of the game
-- |and when user input is " ", returns the last GameState
recursiveLoopOfGameStagesWithoutJerkingAFile :: GameState -> IO GameState
recursiveLoopOfGameStagesWithoutJerkingAFile gameState = do
    () <- renderFrame $ getFrameContents gameState
    userActionLine <- getLine
    if userActionLine == " "
        then do
            let newGameState = updateGameState userActionLine gameState 
            recursiveLoopOfGameStagesWithoutJerkingAFile newGameState
        else return gameState

infiniteStagesGame :: IO ()
infiniteStagesGame = do
    makeFileWithInitialGameState --SAVING TEST GAME STATE AS INITIAL!
    initialGameState <- loadGameFromAFile
    lastGameState <- recursiveLoopOfGameStagesWithoutJerkingAFile initialGameState
    -- when " " unput occured, reqursion ended and we're making an autosave
    saveGameIntoAFile lastGameState


--------------------------------- MAIN -----------------------------------------
main :: IO ()
main = infiniteStagesGame

