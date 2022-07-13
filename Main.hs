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
    , lookupColor
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
import Data.Maybe (fromMaybe)

import System.IO (
    openFile, hGetContents, hClose, IOMode(ReadMode), hPutStr
    , openTempFile
    )
import System.Directory (removeFile, renameFile)
import Text.Read (readMaybe)

import GHC.Stack (HasCallStack)
import Debug.Trace (trace)

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
-- build busstops inside a frame012! (but make their entries in save anyway)
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

pixelFromCursorPoint :: CursorPoint -> GenPixel Char
pixelFromCursorPoint = flip MkGenPixel (lookupColor "cursor") --part. app.

testFrameContents :: FrameContents
testFrameContents = (
    testShapes
    , testBusstops
    , [MkGenPixel (MkIntPoint 0 0) (MkGenColor '5')]
    )

testGameState :: GameStateString
testGameState = makeGameState testFrameContents testGameMeta
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
        concatMap entry (pixels ++ [pixelFromCursorPoint cursorPt]),
        --LAST PIXEL IS A CURSOR PIXEL (last cuz it must have the highest presedence)
        "\n" ++ entryPrefix ++ "CURSOR POINT\nP=", entry cursorPt, "\n",
        "\n" ++ entryPrefix ++ "BUFFERED POINTS\nPTS=", entry bufferedPts, "\n",
        "\n" ++ entryPrefix ++ "STAGE NUM\nN=", entry stageNum, "\n"
        ]

-- |just a GameStateString PARSER (inverse to makeGameState)
-- |algorithm:
-- |1. Obtain a list of entries by splitting GameStateString with "==|>"
-- |2. other stuff...
-- |function uses unsafe head and init!
parseGameState :: GameStateString -> (FrameContents, GameMeta)
parseGameState gameState = (
    (shapes, busstops, pixels),
    (cursor, buffer, stage)
    )
    where
        entries :: [[Line]]
        entries = -- filter (not null) - костыль для удаления пустых линий
            groupBy (\_ line -> not (entryPrefix `isPrefixOf` line)) . filter (not . null) . lines $ gameState

        singleOutTitleLineFromEntry :: [Line] -> (Line, [Line])
        singleOutTitleLineFromEntry testCheck@(headLine:restOfLines)
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
            | otherwise = error $ "Unexpected entry headLine, namely:" ++ headLine ++ "!" ++ (unlines restOfLines) ++ "!" ++ show (length testCheck)
        singleOutTitleLineFromEntryLines [] = error "No Entries found!"

        -- mapped not by "type name" exactly but by value constructor name
        -- :: Map.Map String [String] or Map. titleLine restOfEntryLines
        concatenatedCommonTypeEntriesMappedByTypeName :: Map.Map Line [Line]
        concatenatedCommonTypeEntriesMappedByTypeName = 
            Map.fromListWith (++) (map singleOutTitleLineFromEntry entries)

        -- gewt means Glued Entries Without Titles
        getGewt :: String -> Gewt
        getGewt title = fromMaybe (error ("No such key in entries map. Namely:" ++ title ++ "!"))
            (Map.lookup title concatenatedCommonTypeEntriesMappedByTypeName)

        value :: String -> String --field value in line of Gewt
        value = tail . dropWhile (/= '=')
        -- nameF :: String -> String --field name in line of Gewt
        -- nameF = takeWhile (/= '=')
        ucPt :: (Int, Int) -> IntPoint
        ucPt = uncurry MkIntPoint
        myRead :: (Read a) => String -> Line -> a
        myRead errCode = fromMaybe (error errCode) . readMaybe . value

        --tracedMyRead errCode = trace errCode (myRead errCode) --DIDN'T WORK!

        dummyStr = StreetPP "FUCK" (ucPt (3, 3)) (ucPt (40, 3))

        -------------------------------
        readBuildings :: Gewt -> [Shape]
        readBuildings [] = []
        readBuildings [_] = []
        readBuildings [_, _] = []
        readBuildings (nameLine : p1Line : p2Line : others) =
            Building name pt1 pt2 : readBuildings others
            where 
                name = value nameLine :: String
                pt1 = ucPt (myRead "1" p1Line :: (Int, Int)) -- Syntax need testing
                pt2 = ucPt (myRead "2" p2Line :: (Int, Int))

        readStreets :: Gewt -> [Shape]
        readStreets [] = []
        readStreets [_] = []
        readStreets [_, _] = []
        readStreets (nameLine : p1Line : p2Line : others) =
            StreetPP name pt1 pt2 : readStreets others
            where 
                name = value nameLine :: String
                pt1 = ucPt (myRead "3" p1Line :: (Int, Int)) -- Syntax need testing
                pt2 = ucPt (myRead "4" p2Line :: (Int, Int))

        readRoutes :: Gewt -> [Shape]
        readRoutes [] = []
        readRoutes [_] = []
        readRoutes (numberLine : pointsLine : others) =
            Route number points : readRoutes others
            where 
                number = myRead "5" numberLine :: Int
                coordsOfPoints =  myRead "6" pointsLine :: [(Int, Int)]
                points = map ucPt coordsOfPoints

        -------------------------------
        -- для Busstops, подгруженных с сейва теряется привязка к Street
        -- Но это похер, ведь их можно в любой момент перестроить имея [Street]
        readIntersections :: Gewt -> [Busstop]
        readIntersections [] = []
        readIntersections [_] = []
        readIntersections [_, _] = []
        readIntersections (_ : _ : crdsLine : others) =
            Intersection dummyStr dummyStr (ucPt coords) 
             : readIntersections others
            where 
                coords = myRead "7" crdsLine :: (Int, Int)

        readDeadends :: Gewt -> [Busstop]
        readDeadends [] = []
        readDeadends [_] = []
        readDeadends (_ : coordsFieldLine : otherLines) =
            Deadend dummyStr (ucPt coords) : readDeadends otherLines
            where 
                coords = myRead "8" coordsFieldLine :: (Int, Int)

        readExtraBusstops :: Gewt -> [Busstop]
        readExtraBusstops [] = []
        readExtraBusstops [_] = []
        readExtraBusstops (streetFieldLine : coordsFieldLine : otherLines) =
            Extra dummyStr (ucPt coords) : readExtraBusstops otherLines
            where 
                coords = myRead "9" coordsFieldLine :: (Int, Int)


        ---------------------------
        readPixels :: Gewt -> [GenPixel Char]
        readPixels [] = []
        readPixels [_] = []
        readPixels (pointFieldLine : charFieldLine : otherLines) =
            MkGenPixel (uncurry MkIntPoint coords) (MkGenColor char) : readPixels otherLines
            where 
                coords = myRead "10" pointFieldLine :: (Int, Int)
                char = myRead "11" charFieldLine :: Char
            --zipWith3 (\x y c -> MkGenPixel (MkIntPoint x y) (MkGenColor c))
            --[0..] (repeat 0) "parser func worked" --FOR TESTS

        -------------------------
        readCursor :: Gewt -> CursorPoint
        readCursor (head -> field) = 
            let (x, y) = myRead "12" field :: (Int, Int) 
            in MkIntPoint x y
                
        readBuffer :: Gewt -> [BufferedPoint]
        readBuffer (head -> field) = 
            let coordPairs = myRead "13" field :: [(Int, Int)]
            in map (uncurry MkIntPoint) coordPairs

        readStage :: Gewt -> StageNum
        readStage (head -> field) = myRead "14" field :: Int

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

        --КОСТЫЛЬ! при парсинге GameStateString положение курсора переносим в пиксели!
        --старый пиксель убираем с хвоста и на его место ставим новый
        pixels :: [GenPixel Char]
        pixels = 
            init (readPixels (getGewt "PIXEL")) ++ [pixelFromCursorPoint cursor]

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
-- |4. route last point created with 'r' (route built)
-- |5. buffering a point with 'c' (points buffered one after another)
-- -- |if inputs are (not bb) or (not tt) or (not r...rl) just flush the buffer
updateGameState :: GameStateString -> InputLine -> Maybe GameStateString
updateGameState gs@(parseGameState -> ((shapes, stops, pxs), (cursor, buffer, stage))) inputLine
    = case inputLine of 
        "w" -> Just $ makeGameState (shapes, stops, pxs) (MkIntPoint (iX cursor) (iY cursor - 1), buffer, stage + 1)
        "a" -> Just $ makeGameState (shapes, stops, pxs) (MkIntPoint (iX cursor - 1) (iY cursor), buffer, stage + 1)
        "s" -> Just $ makeGameState (shapes, stops, pxs) (MkIntPoint (iX cursor) (iY cursor + 1), buffer, stage + 1)
        "d" -> Just $ makeGameState (shapes, stops, pxs) (MkIntPoint (iX cursor + 1) (iY cursor), buffer, stage + 1)
        "b" -> Just $ makeGameState (Building ("userInput s" ++ show stage) (buffer !! 0) (buffer !! 1) : shapes, stops, pxs) (cursor, [], stage + 1)
        "t" -> Just $ makeGameState (StreetPP ("userInput s" ++ show stage) (buffer !! 0) (buffer !! 1) : shapes, stops, pxs) (cursor, [], stage + 1)
        "r" -> Just $ makeGameState (Route 0 buffer : shapes, stops, pxs) (cursor, [], stage + 1)
        "c" -> Just $ makeGameState (shapes, stops, pxs) (cursor, cursor:buffer, stage + 1)
        _ -> Nothing        


------------------------- impure functions for main ----------------------------
makeFileWithInitialGameState :: IO ()
makeFileWithInitialGameState = do 
    writeFile gameStateFileName testGameState

loadGameFromAFile :: IO GameStateString
loadGameFromAFile = readFile gameStateFileName

--saveGameIntoAFile :: GameStateString -> IO () --DEPRECATED
--saveGameIntoAFile = writeFile gameStateFileName --part. app.

saveGameIntoANewFileAndDeleteOldFile :: GameStateString -> IO () --DEPRECATED
saveGameIntoANewFileAndDeleteOldFile gameStateString = do 
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle gameStateString
    hClose tempHandle
    removeFile gameStateFileName
    renameFile tempName gameStateFileName
    
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

funcc :: Maybe GameStateString -> GameStateString -> IO GameStateString
funcc (Just newGameStateString) _ = return newGameStateString
funcc _ prevGameStateString = do
    putStrLn "wrong Key, mate"
    return prevGameStateString

-- |if user input is not " ", calls itself to do next stage of the game
-- |and when user input is " ", returns the last GameState
recursiveLoopOfGameStagesWithoutJerkingAFile :: GameStateString -> IO GameStateString
recursiveLoopOfGameStagesWithoutJerkingAFile prevGameStateString = do
    () <- renderFrame $ getFrameContents prevGameStateString
    userActionLine <- getLine
    if userActionLine /= " "
        then do
            let maybeNewGameStateString = updateGameState prevGameStateString userActionLine
            updatedGameStateString <- funcc maybeNewGameStateString prevGameStateString
            saveGameIntoANewFileAndDeleteOldFile updatedGameStateString --FOR FOR LAST STAGE STATE LOGGING IN CASE OF A FAILURE
            recursiveLoopOfGameStagesWithoutJerkingAFile updatedGameStateString
        else return prevGameStateString

infiniteStagesGame :: IO ()
infiniteStagesGame = do
    makeFileWithInitialGameState --SAVING TEST GAME STATE AS INITIAL!
    initialGameState <- loadGameFromAFile
    lastGameState <- recursiveLoopOfGameStagesWithoutJerkingAFile initialGameState
    -- when " " unput occured, reqursion ended and we're making an autosave
    saveGameIntoANewFileAndDeleteOldFile lastGameState


--------------------------------- MAIN -----------------------------------------
main :: IO ()
main = infiniteStagesGame

