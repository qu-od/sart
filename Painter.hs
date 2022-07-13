module Painter
( Entry (entry)
, IntPoint (MkIntPoint, iX, iY)
, GenColor (MkGenColor)
, GenPixel (MkGenPixel)
, Frame
, Shape (EmptyShape, Building, StreetPP, pt1, pt2, Route)
, Busstop (Intersection, Deadend, Extra) -- for advanced routes
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
) where

import Data.List ( 
    nub, sort, reverse, groupBy, intersperse, intercalate
    )
import Data.Char (
    isUpper, isDigit, isLower
    )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Function ( on )
import Data.Maybe (fromMaybe)


---------------------------- TYPECLASSES ---------------------------------------
class Entry a where
    entry :: a -> String

instance Entry Int where
    entry n = show n

instance (Entry a) => Entry [a] where
    entry list = '[' : intercalate "," (map entry list) ++ "]"
    --entry list = concatMap entry list

entryPrefix :: String --prefix of every entry in a file parametrized
entryPrefix = "==|> "


--------------- OLD TYPES (deprecated since 0.0.1) -----------------------------
data Point = MakePoint {getX :: Int, getY :: Int} deriving (Show, Eq, Ord)
--howw auto-deriving works. Which order will points even have?

data Color = MakeColor {symbol :: Char} deriving (Show)

data Pixel = MakePixel {coords :: Point, color :: Color} deriving (Show)


----------------------------- IntPoint type 0.1.1 ------------------------------
-- type needs testing
data IntPoint = MkIntPoint {iX :: Int, iY :: Int}

instance Eq IntPoint where 
    MkIntPoint x1 y1 == MkIntPoint x2 y2 = (x1 == x2) && (y1 == y2)
    MkIntPoint x1 y1 /= MkIntPoint x2 y2 = (x1 /= x2) || (y1 /= y2)
    
-- |to use type as a key in Map, this type must be ordered
instance Ord IntPoint where
    compare (MkIntPoint x1 y1) (MkIntPoint x2 y2)
        | y1 /= y2 = compare y1 y2
        | otherwise = compare x1 x2

--instance Bounded IntPoint where
    --minBound = (head screenXs, head screenYs)
    --maxBound = (last screenXs, last screenYs)

--instance Enum IntPoint where
    -- all possible values are: 
    -- [MakeIP x y | y <- screenYs, x <- screenXs]
    -- and that's an order! (pun intended)

instance Show IntPoint where
    show (MkIntPoint x y) = "IntPoint: x=" ++ show x ++ ", y=" ++ show y

--instance Read IntPoint where
    -- PARSER MODULE NEEDED

instance Entry IntPoint where
    entry (MkIntPoint x y) = concat ["(", show x, ", ", show y, ")"]

-- shorter string for a IntPoint
xy :: IntPoint -> String
xy (MkIntPoint x y) = "X=" ++ show x ++ ", Y=" ++ show y

leftX :: [IntPoint] -> Int
leftX = minimum . map iX

rightX :: [IntPoint] -> Int
rightX = maximum . map iX

upperY :: [IntPoint] -> Int
upperY = minimum . map iY

lowerY :: [IntPoint] -> Int
lowerY = maximum . map iY


--------------------- OTHER TYPES 0.1.1 (Maps and Sets) ------------------------
--------------- GenColor
data GenColor a = MkGenColor a deriving (Show)
-- how that show will be implemented on different types?
-- which types could be a? Other than Char

--------------- GenPixel
data GenPixel a = MkGenPixel {
    point :: IntPoint,
    genColor :: GenColor a
    } 

instance (Show a) => Show (GenPixel a) where
-- constraint shows that type "a" will be put into string somewhere in this function
    show (MkGenPixel pt genColor) = 
        "GenPixel at " ++ xy pt ++ "With GenColor " ++ show genColor

-- shorter string for a GenPixel
gPx :: (Show a) => GenPixel a -> String
gPx (MkGenPixel pt color) = xy pt ++ show color

-- type MapPixel a = Map.Map IntPoint (GenColor a) -- record syntax is illegal here!
-- instance (Show a) => Show (MapPixel a) where -- CANNOT INSTANTIATE TYPE SYNONYMS WTF
     --show pts clrs = show "MapPixel coords"
--showMapPx (Map.Map pt genColor) = xy pt ++ show genColor --WHY Map.Map is not in scope?

toPairs' :: [GenPixel Char] -> [(IntPoint, GenColor Char)]
toPairs' pixels = [(p, color) | (MkGenPixel p color) <- pixels]

instance (Show c) => Entry (GenPixel c) where
    -- constraint on a added because we'll be applying show
        -- to color a in a GenPixel a
    entry (MkGenPixel p (MkGenColor c)) = 
        '\n' : unlines [
            entryPrefix ++ "PIXEL"
            , "P=" ++ entry p
            , "CHAR=" ++ show c
            ]

-------------- Frame
type Frame a = Map.Map IntPoint (GenColor a)
-- type synonyms can't be instantiated

data TestFrame a = MkTestFrame (Map.Map (GenPixel a) Color)
--Map.Map is a TYPE CONSTRUCTOR, not a VALUE CONSTRUCTOR! 

-------------- Direction
data Direction = 
    NoDirection
    | Up
    | Down
    | Left'
    | Right'
    | Naher
    | AdAstra
    deriving (Eq)

instance Show Direction where
    show NoDirection = "There is a direction with a nullary constructor, \
                        \but it says \"NoDirection\" lol"
    show Up = "Upwards"
    show Down = "Downwards"
    show Left' = "to the Left"
    show Right' = "to the Right"
    show Naher = "No luck to you pal. You better stop doing this"
    show AdAstra = "That's some strange nullary value constructor \
                    \of the Direction type you've got there..."

----------------- Polyline
-- Polyline = DATA or TYPE?

---------------- Shape
data Shape = -- GENERALIZE ALL THOSE WITH A BOX AND A POLYLINE
        -- record syntax DISCOURAGED when multiple value constructors are used
        -- ADDING DIFFERENT CONSTRUCTORS FOR A STREET WAS A MISTAKE))
    EmptyShape
    | Building {
        name :: String,
        ulp :: IntPoint, -- upper left point
        lrp :: IntPoint -- lower right point
        }
    | StreetPD { -- DEPRECATED after 0.1.2
        name :: String,
        pt0 :: IntPoint,
        dir :: Direction,
        len :: Int
        }
    | StreetPP {
        name :: String,
        pt1 :: IntPoint,
        pt2 :: IntPoint
        }
    | Route {
        number :: Int, pts :: [IntPoint]
        }

instance Show Shape where 
    show EmptyShape = "Empty Shape"
    show (Building name pt1 pt2) = concat [
        "Building \"", name, "\" between ", xy pt1, "; ", xy pt2
        ]
    show (StreetPD name pt dir len) = concat [
        name, "Street from ", xy pt, show dir, "len=", show len
        ]
    show (StreetPP name pt1 pt2) = concat [
        name, "Street from ", xy pt1, "to ", xy pt2
        ]
    show (Route num pts) = concat [
        "Route #", show num, "Turns: ", show pts
        ]

-- |Function takes both Street value constructors (StreetPD and StreetPP)
-- |But if it is StreetPD, converts it to StreetPP
-- |Костыли тоже надо уметь писать!
ensureStreetPP :: Shape -> Shape
ensureStreetPP st@(StreetPD name pt dir len) = StreetPP name pt (streetPDP2 st)
ensureStreetPP st@StreetPP {} = st
ensureStreetPP _ = error "Street required"

streetPDP2 :: Shape -> IntPoint
streetPDP2 (StreetPD _ (MkIntPoint x y) dir len)
    | dir == Up     = p x (y-d)
    | dir == Down   = p x (y+d)
    | dir == Left'  = p (x-d) y
    | dir == Right' = p (x+d) y
    where 
        p = MkIntPoint
        d = len - 1
streetPDP2 _ = 
    error "streetPDP2 funnction requires StreetPD and not any other Shape"

streetPPLen :: Shape -> Int -- bigPP
streetPPLen (StreetPP _ (MkIntPoint x1 y1) (MkIntPoint x2 y2))
    | y1 == y2 = 1 + abs (x2 - x1) -- horizontal
    | x1 == x2 = 1 + abs (y2 - y1) -- vertical
    | otherwise = error "Diagonal street occured which is forbidden"
streetPPLen _ =
    error "streetPPLen function requires StreetPP and not any other Shape"

-- |single-point street case is not handled specifically
streetAxis :: Shape -> String
streetAxis (StreetPD _ _ dir _)
    | dir == Up     = "oY"
    | dir == Down   = "oY"
    | dir == Left'  = "oX"
    | dir == Right' = "oX"
    | otherwise = error "Unexpected street Direction"
streetAxis (StreetPP _ (MkIntPoint x1 y1) (MkIntPoint x2 y2) )
    | x1 == x2 = "oY"
    | y1 == y2 = "oX"
    | otherwise = error "Unexpected street Direction"
streetAxis _ = streetRequiredError

-- |that's basically a type "method"
-- |function doesn't paint buildings' walls into special color
-- -- |and doesn't render name of a shape
interpolate' :: Shape -> [IntPoint]
interpolate' (Building _ pt1 pt2) = [
    MkIntPoint x y 
    | x <- [leftX pts .. rightX pts], y <- [upperY pts .. lowerY pts]
    ]
    where 
        pts = [pt1, pt2]
interpolate' (Route _ []) = []
interpolate' (Route _ [pt]) = [pt]
interpolate' (Route _ (pt1:pt2:pts)) = 
        init (interpolate' (StreetPP "route section" pt1 pt2))
        ++ interpolate' (Route 0 (pt2:pts))
interpolate' EmptyShape = []
interpolate' (StreetPD _ pt Up len) = 
    map (MkIntPoint (iX pt)) [iY pt - (len-1) .. iY pt]
interpolate' (StreetPD _ pt Down len) =
    map (MkIntPoint (iX pt)) [iY pt .. iY pt + (len-1)]
interpolate' (StreetPD _ pt Left' len) =
    map (`MkIntPoint` iY pt) [iX pt - (len-1) .. iX pt] --INFIX NOTAION INSTEAD OF USING FLIP
interpolate' (StreetPD _ pt Right' len) =
    map (`MkIntPoint` iY pt) [iX pt .. iX pt + (len-1)]
interpolate' st@(StreetPD _ pt _ len) = error "Unexpected Direction"
interpolate' st@(StreetPP _ pt1 pt2)
    | streetAxis st == "oX" = 
        map (`MkIntPoint` iY pt1) [leftX pts .. rightX pts]
    | otherwise = map (MkIntPoint (iX pt1)) [upperY pts .. lowerY pts]
    where 
        pts = [pt1, pt2]

isStreet :: Shape -> Bool
isStreet StreetPD {} = True -- record patterns OH MY!
isStreet StreetPP {} = True
isStreet _ = False

-- which type need to be used?
streetRequiredError :: a
streetRequiredError = error "There was a Shape... But there was no STREET!"

-- NEED TESTING
instance Entry Shape where
    entry (Building name p1 p2) = '\n' : unlines [
        entryPrefix ++ "BUILDING"
        , "NAME=" ++ name --or show name?
        , "P1=" ++ entry p1 
        , "P2=" ++ entry p2
        ]
    entry (StreetPP name p1 p2) = '\n' : unlines [
        entryPrefix ++ "STREET"
        , "NAME=" ++ name --or show name?
        , "P1=" ++ entry p1 
        , "P2=" ++ entry p2
        ]
    entry (Route number pts) = '\n' : unlines [
        entryPrefix ++ "ROUTE"
        , "NUMBER=" ++ show number --or show name?
        , "PTS=[" ++ intercalate "," (map entry pts) ++ "]"
        ]
    entry _ = error "Unexpected Shape value constructor"

-------------- Busstop
data Busstop =
    -- record syntax DISCOURAGED when multiple value constructors are used
    Intersection {
        streetOX :: Shape,
        streetOY :: Shape,
        pPoint :: IntPoint
        } 
    | Deadend { -- every "start" and every "end" of the street 
        street :: Shape,
        pPoint :: IntPoint
        }
    | Extra { -- in the middle of the street --BAD NAMING NEPONYATNO NIHUIA
        street :: Shape,
        pPoint :: IntPoint
        }

instance Show Busstop where
    show (Intersection st1 st2 pt) = concat [
        "Place \"Intersection\" of ",
        name st1, " st. and ",
        name st2, " st. at",
        xy pt
        ]
    show (Deadend st pt) =
        "Place \"Deadend\" of " ++ name st ++ " st. at " ++ xy pt
    show (Extra st pt) = 
        "Place \"ExtraBusstop\" on a " ++ name st ++ " st. at " ++ xy pt

-- NEED TESTING
instance Entry Busstop where
    entry (Intersection st1 st2 pt) = '\n' : unlines [
        entryPrefix ++ "INTERSECTION"
        , "STREET1=" ++ name st1 --id would be better but streets don't have ones
        , "STREET2=" ++ name st2 
        , "P=" ++ entry pt
        ]
    entry (Deadend st pt) = '\n' : unlines [
        entryPrefix ++ "DEADEND"
        , "STREET=" ++ name st --or show name?
        , "P=" ++ entry pt
        ]
    entry (Extra st pt) = '\n' : unlines [
        entryPrefix ++ "EXTRA"
        , "STREET=" ++ name st --or show name?
        , "P=" ++ entry pt
        ]
        

--------------------------------- CONSTS ---------------------------------------
screenWidth :: Int
screenWidth = 160

screenHeight :: Int
screenHeight = 40

screenSize :: (Int, Int)
screenSize = (screenWidth, screenHeight)

screenXs :: [Int]
screenXs = [0 .. screenWidth - 1]

screenYs :: [Int]
screenYs = [0 .. screenHeight - 1]

palette :: Map.Map String (GenColor Char)
palette = Map.fromList [
    ("background",       c '.'),
    ("building wall",    c '%'),
    ("building body",    c ' '),
    ("street",           c '#'),
    ("route",            c '^'), -- if it won't be a number-char one time
    ("intersection",     c '+'),
    ("deadend",          c '~'),
    ("extra busstop",    c '*'),
    ("route vertical",   c '|'), -- for advanced routes maybe
    ("route horizontal", c '-'), -- for advanced routes maybe
    ("cursor",           c '■')
    ]
    where c = MkGenColor

backgroundColor00x :: Color --deprecated
backgroundColor00x = MakeColor ' '

backgroundPxs :: [GenPixel Char]
backgroundPxs = [
    MkGenPixel (MkIntPoint x y) (lookupColor "background") 
    | y <- screenYs, x <- screenXs
    ]

lookupColor :: String -> GenColor Char
lookupColor string = 
    fromMaybe (error "No color in palette for that stuff you requested")
    (Map.lookup string palette)

colored :: String -> [IntPoint] -> [GenPixel Char]
colored colorKey = map (`MkGenPixel` lookupColor colorKey) --oh my

--------------------------------- WHEELS ---------------------------------------
scnd :: (a, b, c) -> b
scnd (_, y, _) = y

thrd :: (a, b, c) -> c
thrd (_, _, z) = z

-- Use foldl instead
concatStrings :: String -> [String] -> String
concatStrings accumulator [] = accumulator --breaks the loop
--concatStrings "" strings = concatStrings ("" ++ head strings) (tail strings) --starts folding CAN BE REMOVED
concatStrings accumulator strings = 
    concatStrings (accumulator ++ head strings) (tail strings) --ordinary iteration

findIndexOfTheFirstMatch :: (Eq a) => a -> [a] -> Int
findIndexOfTheFirstMatch x xs = 
    head [i | i <- [0 .. length xs - 1], xs !! i == x]

myStoneSortWithDupesDeletion :: [Int] -> [Int] -> [Int] -- пока не понадобилось для версии 0.0.3
myStoneSortWithDupesDeletion sorted [] = sorted
myStoneSortWithDupesDeletion sorted unsorted = 
    myStoneSortWithDupesDeletion (bubble:sorted) (headingPart ++ trailingPart)
    where
        bubble = maximum unsorted
        holeIndex = findIndexOfTheFirstMatch bubble unsorted
        headingPart = take holeIndex unsorted 
        trailingPart = drop (holeIndex + 1) unsorted
        updatedSorted = if bubble /= head sorted then bubble:sorted else sorted

paint :: Char -> [Point] -> [Pixel]
paint symbol points = [MakePixel pt (MakeColor symbol) | pt <- points]

--pixelsCoords :: [Pixel] -> [Point] --стало ненужно))0
--pixelsCoords pixels = [coords px | px <- pixels]

colorInThePoint :: Point -> [Pixel] -> Color
colorInThePoint point pixels = color (pixels !! i)
    where i = findIndexOfTheFirstMatch point (map coords pixels)


---------------------------- MONOCHROME SCREEN ---------------------------------
-- func monochromeScreen paints points turning them into pixels.
    -- Since it's monochrome, there is only one color to paint with
monochromeScreen :: [(Int, Int)] -> [(Int, Int, Color)]
monochromeScreen pixelsToPaint = 
  [(x, y, chooseMonochromeSymbol (x, y)) | y <- ys, x <- xs]
    where 
        xs = [0 .. screenWidth-1 ]
        ys = [0 .. screenHeight-1]
        chooseMonochromeSymbol coordPair = if coordPair `elem` pixelsToPaint
            then MakeColor ' '
            else backgroundColor00x

monochromeFrame :: [(Int, Int)] -> [String]
monochromeFrame pixelsToPaint = [line y | y <- [0..screenHeight-1]]
    where line y = [
            symbol | 
            pixel@(_, _, MakeColor symbol) <- monochromeScreen pixelsToPaint,
            scnd pixel == y
            ]


----------------------- MULTICOLOR IMPLEMENTATION ------------------------------
line' :: Int -> [Pixel] -> [Pixel]
line' y rawColoredPixels = [
    if pixelWasColored x then coloredPixel x else backgroundPixel x | x <- screenXs --ВЗЯЛ ЛИСТ В ЛИСТ И СОСАЛ ПОЛЧАСА
    ]
    where
        pt x = MakePoint x y
        rawColoredPixelsInLine y = --filtering by pixel.y
            [px | px <- rawColoredPixels, getY (coords px) == y]
        pixelWasColored x = 
            pt x `elem` map coords (rawColoredPixelsInLine y)
        coloredPixel x = 
            MakePixel (pt x) (colorInThePoint (pt x) (rawColoredPixelsInLine y))
        backgroundPixel x = 
            MakePixel (pt x) backgroundColor00x
        
formPixelMatrix :: [Pixel] -> [[Pixel]]
formPixelMatrix rawColoredPixels = [line' y rawColoredPixels | y <- screenYs]
        -- first pixel that matches (x0, y0) coords is chosen.
        -- So pixel in (x0, y0) will be painted in correspondent color

formStrings :: [[Pixel]] -> [String]
formStrings pixelMatrix = 
    [map (symbol . color) pixelLine | pixelLine <- pixelMatrix] -- ДЫААААААА

-- DEPRECATED
frame :: [[Pixel]] -> String
frame figures = unlines $ formStrings $ formPixelMatrix $ concat figures


--------------------- 0.1.0 FRAME IMPLEMENTATION (with HOFs) -------------------
-- "frame" function alghorithm:
-- 1. get [figure] (figure = [Pixel]) 
    -- add backgroundPixels as a figure with the lowest precedence for its pixels
-- 2. flatten pixels in those figures, sort them and remove dupes
    -- (if there is a collision and different figures want to occupy the same point
    -- figure which was defined earlier in the list of figures wins)
-- 3. form a matrix from that brushed list of pixels
-- 4. form one multiline string for the pixels matrix

backgroundPixels :: [Pixel]
backgroundPixels = [
    MakePixel (MakePoint x y) backgroundColor00x | x <- screenXs, y <- screenYs
    ]

quickSort :: (Ord a, Eq a) => [a] -> [a] 
quickSort [] = []
quickSort [x] = [x]
quickSort (pivot:xs) = 
    quickSort (filter (<=pivot) xs) ++ [pivot] ++ quickSort (filter (>pivot) xs)

quickSortWithDupesDeletion :: (Ord a) => [a] -> [a] 
quickSortWithDupesDeletion [] = []
quickSortWithDupesDeletion [x] = [x]
quickSortWithDupesDeletion (pivot:xs) = lessers ++ [pivot] ++ greaters
    where
        lessers  = quickSortWithDupesDeletion (filter (<pivot) xs)
        greaters = quickSortWithDupesDeletion (filter (>pivot) xs)

-- arrangePixels is a pixel quicksort by a coords tuple with dupes removal
arrangePixels :: [Pixel] -> [Pixel]
arrangePixels [] = []
arrangePixels [px] = [px]
arrangePixels (pivotPx:pxs) = lesserPixels ++ [pivotPx] ++ greaterPixels
    where
        yx (MakePixel (MakePoint x y) _)  = (y, x) --cuz we need to reverse coordss
        lesserPixels =
            arrangePixels $ filter (\px -> yx px < yx pivotPx) pxs
        greaterPixels =
            arrangePixels $ filter (\px -> yx px > yx pivotPx) pxs

dropOutOfBoundsPixels :: [Pixel] -> [Pixel]
dropOutOfBoundsPixels =
    filter $ \px -> (getX (coords px) < screenWidth) && (getY (coords px) < screenHeight)

-- NEED TESTING!
frameMatrix :: [Pixel] -> [[Pixel]]
frameMatrix [] = [[]] --NEED FIX PROBABLY!
frameMatrix [pxN] = [[pxN]] --NEED FIX PROBABLY!
frameMatrix (px0:px1:pxs)
    | isLineContinues = (px0 : head (frameMatrix (px1:pxs))) : drop 1 (frameMatrix (px1:pxs)) --same line continued
    | otherwise = [px0] : frameMatrix (px1:pxs) --new line started
    where isLineContinues = getY (coords px0) == getY (coords px1)

pixelsLineToString :: [Pixel] -> String
pixelsLineToString = foldr (\px acc -> symbol (color px):acc) ""
--pixelsLineToString = foldr (\(MakePixel _ (MakeColor symbol)) acc -> symbol:acc) "" --suboptimal variant

matrixToString :: [[Pixel]] -> String
matrixToString pxsMatrix = unlines $ map pixelsLineToString pxsMatrix

-- takes a list of (completely) unsorted pixels
frame01 :: [Pixel] -> String
frame01 figures = 
    (matrixToString . frameMatrix . arrangePixels . dropOutOfBoundsPixels)
    (figures ++ backgroundPixels)


---------------- 0.1.2 FRAME (Data Modules and verbose types) ------------------
------ frame012 ALGORITHM ------
-- |1. Get Shapes and Places (StreetPDs prohibited!)
-- |2. Form colored pixels for every Shape and Place
-- -- |Names could be rendered at that point (but won't)
-- -- |Also add background Pixels there
-- |3. Dump them in a Frame
-- -- |Duplicates are removed and Pixels ordered by coords because
-- -- -- |Frame Char = Map IntPoint (GenColor Char)
-- |4. Take only that pixels which are in-bouds of a frame coord matrix
-- |5. Form multiline string from a Frame

pixelsFromShape :: Shape -> [GenPixel Char]
pixelsFromShape bdg@Building {} = colored "building wall" (interpolate' bdg)
pixelsFromShape st@StreetPP {} = colored "street" (interpolate' st)
pixelsFromShape route@Route {} = colored "route" (interpolate' route)
pixelsFromShape StreetPD {} =
    error "StreetPDs prohibited. Convert them to StreetPPs"
pixelsFromShape _ = error "Unexpected Shape recieved"

-- |won't do a function for retrieving single pixel from a busstop because of
-- -- |generalization
-- |I mean may be we'll want a list of pixels
-- -- |from a single busstop in the future!
pixelsFromBusstop :: Busstop -> [GenPixel Char]
pixelsFromBusstop (Intersection _ _ p) =
    [MkGenPixel p $ lookupColor "intersection"]
pixelsFromBusstop (Deadend _ p) = [MkGenPixel p $ lookupColor "deadend"]
pixelsFromBusstop (Extra _ p) = [MkGenPixel p $ lookupColor "extra busstop"]

-------frame 012 pipeline
dumpPixels :: ([Shape], [Busstop], [GenPixel Char]) -> [GenPixel Char]
dumpPixels (shapes, busstops, miscPixels) = 
    -- Order in concat matters!
        -- first components have lower preсedence in rendering
    backgroundPxs 
    ++ concatMap pixelsFromShape shapes
    ++ concatMap pixelsFromBusstop busstops
    ++ miscPixels

-- |Map.fromList should also remove duplicates
frameMapFromPixels :: [GenPixel Char] -> Frame Char
frameMapFromPixels = Map.fromList . toPairs'

cropFrameBounds :: Frame Char -> Frame Char
cropFrameBounds = Map.fromList . filter inBounds . Map.toList
    where inBounds (MkIntPoint x y, _) = x `elem` screenXs && y `elem` screenYs

-- |Convert "Frame" Map to list of pairs
-- -- |Namely pairs of (IntPoint, GenColor Char)
-- |Group that list by iY of the Point. We'll get Matrix of pairs Then
-- |Flatten chars in every line of pair-matrix. We'll get list of strings
-- |Join strings with newline chars. We'll get a multiline string to show
showFrame :: Frame Char -> String
showFrame frame = ans
    where
        pixelPairs = Map.toList frame
        matrixOfPixelPairs = 
            groupBy (\(p1, _) (p2, _) -> iY p1 == iY p2) pixelPairs
        pixelPairsListToLine = 
            map (\(_, MkGenColor char) -> char)
        ans = unlines $ map pixelPairsListToLine matrixOfPixelPairs

---------frame012
frame012 :: ([Shape], [Busstop], [GenPixel Char]) -> String
frame012 = showFrame . cropFrameBounds . frameMapFromPixels . dumpPixels
