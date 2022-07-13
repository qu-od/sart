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
    , entryPrefix
    )

import Shapes
    ( testShapes
    , testBusstops
    , testMiscPixels
    , testPoints
    )

import Control.Monad 
    ( when
    , sequence
    , forM
    , mapM
    , forever
    )

import Data.List (groupBy, isPrefixOf, isSuffixOf)
import qualified Data.Map as Map
import System.IO (openFile, hGetContents, hClose, IOMode(ReadMode))
import Data.Maybe (fromMaybe)

--import GHC.Stack (HasCallStack)

------------------------------ META --------------------------------------------
-- use functors, applicatives and monads (Maybe, List and IOs)
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
    -- bind them to street grid (0.2.1)
    -- render in parallel to the streets and to each other (0.2.2)
    -- autogenerate route from point A to point B (0.2.3)
-- animate (0.3....)
    -- busses traffic
    -- other traffic
    -- responsive traffic (traffic jams must occur)
--ENOUGH.

------------------------------- TYPES ------------------------------------------
type Line = String
type Gewt = [Line] -- Glued Entries Without Titles (for game save parser)

type InputLine = String

type GameStateString = String --contains entries of FrameContents' and GameMeta's elements

type FrameContents = ([Shape], [Busstop], [GenPixel Char])
type GameMeta = (CursorPoint, [BufferedPoint], StageNum)

type CursorPoint = IntPoint
type BufferedPoint = IntPoint
type StageNum = Int


------------------------------ test functions ----------------------------------
--formFrameWithUserBusstop :: IntPoint -> String
--formFrameWithUserBusstop intPoint =
    --frame012 (testShapes, testBusstops ++ [Extra EmptyShape intPoint], [])

--formFrame :: String
--formFrame = frame012 (testShapes, testBusstops, [])

--uglyLoadGameFromAFile :: IO GameStateString --DEPRECATED
--uglyLoadGameFromAFile = do
    --handle <- openFile gameStateFileName ReadMode 
    --contents <- hGetContents handle --unwrap IO String into String
    --hClose handle
    --return contents --wrap String to IO String

--main = forever $ do
    --movementString <- getLine
    --let newPoint = updateCursor startingPoint (head movementString) --head is unsafe
        -- save point state in a FILE tonight!! And also a frame state
    --putStrLn $ formFrameWithUserBusstop newPoint

-------------------------- pure functions for main -----------------------------
gameStateFileName :: String
gameStateFileName = "game state.txt"

startingPoint :: IntPoint
startingPoint = MkIntPoint 0 0

updateCursor :: CursorPoint -> InputLine -> CursorPoint
updateCursor (MkIntPoint x y) iLine
    | iLine == "a" = MkIntPoint (x - 1) y
    | iLine == "d" = MkIntPoint (x + 1) y
    | iLine == "w" = MkIntPoint x (y - 1)
    | iLine == "s" = MkIntPoint x (y + 1)
    | otherwise = error $ "Wrong line for the movement direction." ++ iLine

testFrameContents :: FrameContents
testFrameContents = (
    testShapes
    , testBusstops
    , [MkGenPixel (MkIntPoint 0 0) (MkGenColor '5')]
    )

makeTestGameState :: GameStateString
makeTestGameState = makeGameState testFrameContents testGameMeta
    where 
        testFrameContents = (testShapes, testBusstops, testMiscPixels)
        testGameMeta = (testCursorPoint, testBufferedPoints, testSatgeNum)
        testCursorPoint = startingPoint 
        testBufferedPoints = testPoints
        testSatgeNum = 0

-- |USE BINARY DATA STRUCTURE FOR GameState INSTEAD OF PLANE String LATER
makeGameState :: FrameContents -> GameMeta -> GameStateString
makeGameState (shapes, busstops, pixels) (cursorPt, bufferedPts, stageNum) = 
    concat [
        -- unlines is excessive cuz entries
        -- are already multiline and also with '\n' at head
        concatMap entry shapes,
        concatMap entry busstops,
        concatMap entry pixels,
        "\n" ++ entryPrefix ++ "CURSOR POINT\nP=", entry cursorPt, "\n",
        "\n" ++ entryPrefix ++ "BUFFERED POINTS\nPTS=", entry bufferedPts, "\n",
        "\n" ++ entryPrefix ++ "STAGE NUM\nN=", entry stageNum, "\n"
        ]

-- |just a GameStateString PARSER (inverse to makeGameState)
-- |algorithm:
-- |1. Obtain a list of entries by splitting GameStateString with "==|>"
-- |2. other stuff...
-- |function uses unsafe1 head
parseGameState :: GameStateString -> (FrameContents, GameMeta)
parseGameState gameState = (
    (shapes, busstops, pixels),
    (cursor, buffer, stage)
    )
    where
        entries :: [[Line]]
        entries = 
            groupBy (\_ line -> not (entryPrefix `isPrefixOf` line)) . lines $ gameState

        singleOutTitleLineFromEntry :: [Line] -> (Line, [Line])
        singleOutTitleLineFromEntry (headLine:restOfLines)
            | "BUILDING"        `isSuffixOf` headLine = ("BUILDING"        , restOfLines)
            | "STREET"          `isSuffixOf` headLine = ("STREET"          , restOfLines)
            | "ROUTE"           `isSuffixOf` headLine = ("ROUTE"           , restOfLines)
            | "INTERSECTION"    `isSuffixOf` headLine = ("INTERSECTION"    , restOfLines)
            | "DEADEND"         `isSuffixOf` headLine = ("DEADEND"         , restOfLines)
            | "EXTRA"           `isSuffixOf` headLine = ("EXTRA"           , restOfLines)
            | "PIXEL"           `isSuffixOf` headLine = ("PIXEL"           , restOfLines)
            | "CURSOR POINT"    `isSuffixOf` headLine = ("CURSOR POINT"    , restOfLines)
            | "BUFFERED POINTS" `isSuffixOf` headLine = ("BUFFERED POINTS" , restOfLines)
            | "STAGE NUM"       `isSuffixOf` headLine = ("STAGE NUM"       , restOfLines)
            | otherwise = error "Unexpected entry headLine"
        singleOutTitleLineFromEntryLines [] = error "No Entries found!"

        -- mapped not by "type name" exactly but by value constructor name
        -- :: Map.Map String [String] or Map. titleLine restOfEntryLines
        concatenatedCommonTypeEntriesMappedByTypeName :: Map.Map Line [Line]
        concatenatedCommonTypeEntriesMappedByTypeName = 
            Map.fromListWith (++) (map singleOutTitleLineFromEntry entries)

        -- gewt means Glued Entries Without Titles
        getGewt :: String -> Gewt
        getGewt title = fromMaybe (error "No such key in entries map")
            (Map.lookup title concatenatedCommonTypeEntriesMappedByTypeName)

        value :: String -> String --field value in line of Gewt
        value = tail . dropWhile (/= '=') 
        name :: String -> String --field name in line of Gewt
        name = takeWhile (/= '=')

        -------------------------------
        readBuildings :: Gewt -> [Shape]
        readBuildings gewt = []

        readStreets :: Gewt -> [Shape]
        readStreets gewt = []

        readRoutes :: Gewt -> [Shape]
        readRoutes gewt = []

        -------------------------------
        readIntersections :: Gewt -> [Busstop]
        readIntersections gewt = []

        readDeadends :: Gewt -> [Busstop]
        readDeadends gewt = []

        readExtraBusstops :: Gewt -> [Busstop]
        readExtraBusstops gewt = []

        ---------------------------
        readPixels :: Gewt -> [GenPixel Char]
        readPixels gewt = []

        -------------------------
        readCursor :: Gewt -> CursorPoint
        readCursor (head -> field) = 
            let (x, y) = read (value field) :: (Int, Int) 
            in MkIntPoint x y
                
        readBuffer :: Gewt -> [BufferedPoint]
        readBuffer (head -> field) = 
            let coordPairs = read (value field) :: [(Int, Int)]
            in map (uncurry MkIntPoint) coordPairs

        readStage :: Gewt -> StageNum
        readStage (head -> field) = read (value field) :: Int

        ------------------------------
        shapes :: [Shape]
        shapes = 
            readBuildings (getGewt "BUILDING")
            ++ readStreets (getGewt "STREET")
            ++ readRoutes (getGewt "ROUTE")

        busstops :: [Busstop]
        busstops = 
            readIntersections (getGewt "INTERSECTION")
            ++ readDeadends (getGewt "DEADEND")
            ++ readExtraBusstops (getGewt "EXTRA")

        pixels :: [GenPixel Char]
        pixels = readPixels (getGewt "PIXEL")

        cursor :: CursorPoint
        cursor = readCursor (getGewt "CURSOR POINT")

        buffer :: [BufferedPoint]
        buffer = readBuffer (getGewt "BUFFERED POINTS")

        stage :: StageNum
        stage = readStage (getGewt "STAGE NUM")


getFrameContents :: GameStateString -> FrameContents
getFrameContents gameState = fst $ parseGameState gameState

-- |How the game state could be updated? (" " just quits the game completely)
-- |1. cursor moved with 'w' 'a' 's' 'd' inputs (cursor coords updated)
-- |2. building point created with 'b' (p1 - buffered, p2 - built)
-- |3. street point created with 't' (p1 - buffered, p2 - built)
-- |4. route point created with 'r' (points buffered one after another)
-- |5. route last point created with 'l' (route built)
-- -- |if inputs are (not bb) or (not tt) or (not r...rl) just flush the buffer
updateGameState :: GameStateString -> InputLine -> GameStateString
updateGameState (parseGameState -> ((shapes, stops, pxs), (cursor, buffer, stage))) inputLine
    = case inputLine of 
        "w" -> makeGameState (shapes, stops, pxs) (MkIntPoint (iX cursor) (iY cursor - 1), buffer, stage + 1)
        "a" -> makeGameState (shapes, stops, pxs) (MkIntPoint (iX cursor - 1) (iY cursor), buffer, stage + 1)
        "s" -> makeGameState (shapes, stops, pxs) (MkIntPoint (iX cursor) (iY cursor + 1), buffer, stage + 1)
        "d" -> makeGameState (shapes, stops, pxs) (MkIntPoint (iX cursor + 1) (iY cursor), buffer, stage + 1)
        "b" -> makeGameState (Building ("userInput s" ++ show stage) (buffer !! 0) (buffer !! 1) : shapes, stops, pxs) (cursor, [], stage + 1)
        "t" -> makeGameState (StreetPP ("userInput s" ++ show stage) (buffer !! 0) (buffer !! 1) : shapes, stops, pxs) (cursor, [], stage + 1)
        "r" -> makeGameState (shapes, stops, pxs) (cursor, cursor:buffer, stage + 1)
        "l" -> makeGameState (Route 0 buffer : shapes, stops, pxs) (cursor, [], stage + 1)
        _ -> error "Unexpected input line"        


------------------------- impure functions for main ----------------------------
makeFileWithInitialGameState :: IO ()
makeFileWithInitialGameState = do 
    saveGameIntoAFile makeTestGameState

-- TEST THIS GOOD PRACTICE
--usingTempFile :: IO ()
--usingTempFile = 

saveGameIntoAFile :: GameStateString -> IO ()
saveGameIntoAFile = writeFile gameStateFileName --part. app.

loadGameFromAFile :: IO GameStateString
loadGameFromAFile = readFile gameStateFileName
    
renderFrame :: FrameContents -> IO ()
renderFrame fContents = putStrLn $ frame012 fContents

-- |Load GameStateString (GameStateString is just a String)
-- |Render Frame with FrameContents encoded in a GameState
-- |get user input
-- |update GameState accordingly to user input
-- |save new gameState
--singleStageGame :: IO () -- DEPRECATED
--singleStageGame = do
    --gameState <- loadGameFromAFile
    --() <- renderFrame $ getFrameContents gameState
    --userActionLine <- getLine
    --let newGameState = updateGameState gameState userActionLine
    --saveGameIntoAFile newGameState

-- |if user input is not " ", calls itself to do next stage of the game
-- |and when user input is " ", returns the last GameState
recursiveLoopOfGameStagesWithoutJerkingAFile :: GameStateString -> IO GameStateString
recursiveLoopOfGameStagesWithoutJerkingAFile gameState = do
    () <- renderFrame $ getFrameContents gameState
    userActionLine <- getLine
    if userActionLine /= " "
        then do
            let newGameState = updateGameState userActionLine gameState 
            saveGameIntoAFile newGameState --DO FILE SAVE AS A BACKUP JUST IN CASE (TEST)
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

