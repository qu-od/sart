module Painter
( Point (MakePoint)
, Color (MakeColor)
, Pixel (MakePixel)
, frame01
, paint
) where

import Data.List ( 
    nub, sort, reverse
    )
import Data.Char (
    isUpper, isDigit, isLower
    )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Function ( on )

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

-- shorter string for a IntPoint
xy :: IntPoint -> String
xy (MkIntPoint x y) = "X=" ++ show x ++ ", Y=" ++ show y

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

-------------- Frame
type Frame a = Map.Map (GenPixel a) Color     
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

---------------- Shape
data Shape = 
        -- All shapes could've been implemented with maps
        --(Map Name|Number Points|Whatever)
    EmptyShape 
    | Building {
        name :: String,
        pt1 :: IntPoint,
        pt2 :: IntPoint
        }
    | StreetPD {
        name :: String,
        pt :: IntPoint,
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
--how do we merge values of (Map Point Color) then?

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

-------------- Place
data Place = 
    Intersection {
        street1 :: Shape,
        street2 :: Shape,
        iPoint :: IntPoint
        } 
    | Deadend { -- every "start" and every "end" of the street 
        street :: Shape,
        dPoint :: IntPoint
        }
    | Busstop { -- on the street
        street :: Shape,
        bPoint :: IntPoint
        }

instance Show Place where
    show (Intersection st1 st2 pt) = concat [
        "Place \"Intersection\" of ",
        name st1, "st. and ",
        name st2, "st. at",
        xy pt
        ]
    show (Deadend st pt) =
        "Place \"Deadend\" of " ++ name st ++ "st. at " ++ xy pt
    show (Busstop st pt) = 
        "Place \"Busstop\" on a " ++ name st ++ "st. at " ++ xy pt


--------------------------------- CONSTS ---------------------------------------
screenWidth :: Int
screenWidth = 180

screenHeight :: Int
screenHeight = 20

backgroundColor :: Color
backgroundColor = MakeColor '.'

defaultBackgroundColor :: Color
defaultBackgroundColor = MakeColor '-'

defaultColor :: Color
defaultColor = MakeColor '@'

screenSize :: (Int, Int)
screenSize = (screenWidth, screenHeight)

screenXs :: [Int]
screenXs = [0 .. screenWidth - 1]

screenYs :: [Int]
screenYs = [0 .. screenHeight - 1]


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
            then defaultColor
            else defaultBackgroundColor

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
            MakePixel (pt x) backgroundColor
        
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
    MakePixel (MakePoint x y) backgroundColor | x <- screenXs, y <- screenYs
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
    (matrixToString . frameMatrix . arrangePixels . dropOutOfBoundsPixels) (figures ++ backgroundPixels)


---------------- 0.1.2 FRAME (Data Modules and verbose types) ------------------
foo :: String
foo = "bar" -- new branch test
