{-# LANGUAGE ViewPatterns #-}

module GameState
( Entry (entry)
, GameStateString
, FrameContents
, GameMeta
, InputLine
, entryPrefix
, getFrameContents
, updateGameState
, testGameState
) where

import Pixels
    ( IntPoint (MkIntPoint, iX, iY)
    , GenColor (MkGenColor)
    , GenPixel (MkGenPixel)
    , lookupColor
    , testPixels
    , testPoints
    )

import Shapes
    ( Shape (
        Building,
        Street, stName
        )
    , testShapes
    )

import Busstops
    ( Busstop (
        Intersection,
        Deadend,
        Extra
        )
    , testBusstops
    )

import Data.List
    ( groupBy
    , isPrefixOf
    , isSuffixOf
    , intercalate
    )
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)


---------------------------------- TYPES ---------------------------------------
type Line = String
type Gewt = [Line] -- Glued Entries Without Titles (for game save parser)

type GameStateString = String --contains entries of FrameContents' and GameMeta's elements

type FrameContents = ([Shape], [Busstop], [GenPixel Char])
type GameMeta = (CursorPoint, [BufferedPoint], StageNum)

type CursorPoint = IntPoint
type BufferedPoint = IntPoint
type StageNum = Int

type InputLine = String


------------------- ENTRY TYPECLASS and its instances --------------------------
class Entry a where
    entry :: a -> String

instance Entry Int where
    entry n = show n

instance (Entry a) => Entry [a] where
    entry list = '[' : intercalate "," (map entry list) ++ "]"
    --entry list = concatMap entry list

entryPrefix :: String --prefix of every entry in a file parametrized
entryPrefix = "==|> "

instance Entry IntPoint where
    entry (MkIntPoint x y) = concat ["(", show x, ", ", show y, ")"]

instance (Show c) => Entry (GenPixel c) where
    -- constraint on a added because we'll be applying show
        -- to color a in a GenPixel a
    entry (MkGenPixel p (MkGenColor c)) = 
        '\n' : unlines [
            entryPrefix ++ "PIXEL"
            , "P=" ++ entry p
            , "CHAR=" ++ show c
            ]

instance Entry Shape where
    entry (Building name p1 p2) = '\n' : unlines [
        entryPrefix ++ "BUILDING"
        , "NAME=" ++ name --or show name?
        , "P1=" ++ entry p1 
        , "P2=" ++ entry p2
        ]
    entry (Street name p1 p2) = '\n' : unlines [
        entryPrefix ++ "STREET"
        , "NAME=" ++ name --or show name?
        , "P1=" ++ entry p1 
        , "P2=" ++ entry p2
        ]

instance Entry Busstop where
    entry (Intersection st1 st2 pt) = '\n' : unlines [
        entryPrefix ++ "INTERSECTION"
        , "STREET1=" ++ stName st1 --id would be better but streets don't have ones
        , "STREET2=" ++ stName st2 
        , "P=" ++ entry pt
        ]
    entry (Deadend st pt) = '\n' : unlines [
        entryPrefix ++ "DEADEND"
        , "STREET=" ++ stName st --or show name?
        , "P=" ++ entry pt
        ]
    entry (Extra st pt) = '\n' : unlines [
        entryPrefix ++ "EXTRA"
        , "STREET=" ++ stName st --or show name?
        , "P=" ++ entry pt
        ]
        
----------------------------- CONSTANTS ----------------------------------------
startingPoint :: IntPoint
startingPoint = MkIntPoint 0 0

---------------------------- MISC FUNCS ----------------------------------------
pixelFromCursorPoint :: CursorPoint -> GenPixel Char
pixelFromCursorPoint = flip MkGenPixel (lookupColor "cursor") --part. app.

testGameState :: GameStateString
testGameState = makeGameState testFrameContents testGameMeta
    where 
        testFrameContents = (testShapes, testBusstops, testPixels)
        testGameMeta = (testCursorPoint, testBufferedPoints, testSatgeNum)
        testCursorPoint = startingPoint 
        testBufferedPoints = testPoints
        testSatgeNum = 0


--------------------------- SAVE FUNCS -----------------------------------------
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


---------------------- LOAD (PARSER) FUNCS -------------------------------------

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

        dummyStr = Street "FUCK" (ucPt (3, 3)) (ucPt (40, 3))

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
            Street name pt1 pt2 : readStreets others
            where 
                name = value nameLine :: String
                pt1 = ucPt (myRead "3" p1Line :: (Int, Int)) -- Syntax need testing
                pt2 = ucPt (myRead "4" p2Line :: (Int, Int))


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


--------------------------- GAME STATE UPDATER ---------------------------------

-- |How the game state could be updated? (" " just quits the game completely)
-- |1. cursor moved with 'w' 'a' 's' 'd' inputs (cursor coords updated)
-- |2. building point created with 'b' (p1 - buffered, p2 - built)
-- |3. street point created with 't' (p1 - buffered, p2 - built)
-- |4. buffering a point with 'c' (points buffered one after another)
-- -- |if inputs are (not bb) or (not tt) or (not r...rl) just flush the buffer
updateGameState :: GameStateString -> InputLine -> Maybe GameStateString
updateGameState gs@(parseGameState -> ((shapes, stops, pxs), (cursor, buffer, stage))) inputLine
    = case inputLine of 
        "w" -> Just $ makeGameState (shapes, stops, pxs) (MkIntPoint (iX cursor) (iY cursor - 1), buffer, stage + 1)
        "a" -> Just $ makeGameState (shapes, stops, pxs) (MkIntPoint (iX cursor - 1) (iY cursor), buffer, stage + 1)
        "s" -> Just $ makeGameState (shapes, stops, pxs) (MkIntPoint (iX cursor) (iY cursor + 1), buffer, stage + 1)
        "d" -> Just $ makeGameState (shapes, stops, pxs) (MkIntPoint (iX cursor + 1) (iY cursor), buffer, stage + 1)
        "b" -> Just $ makeGameState (Building ("userInput s" ++ show stage) (buffer !! 0) (buffer !! 1) : shapes, stops, pxs) (cursor, [], stage + 1)
        "t" -> Just $ makeGameState (Street ("userInput s" ++ show stage) (buffer !! 0) (buffer !! 1) : shapes, stops, pxs) (cursor, [], stage + 1)
        "c" -> Just $ makeGameState (shapes, stops, pxs) (cursor, cursor:buffer, stage + 1)
        _ -> Nothing        
