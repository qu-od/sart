{-# LANGUAGE ViewPatterns #-}
module Painter
( Point (MakePoint)
, Color (MakeColor)
, Pixel (MakePixel)
, frame01
, paint
) where
import Data.List (elemIndex)
import GHC.Stack (HasCallStack)
import Data.Maybe (fromMaybe)


--------------------------------- TYPES ----------------------------------------
data Point = MakePoint {getX :: Int, getY :: Int} deriving (Show, Eq, Ord)

data Color = MakeColor {symbol :: Char} deriving (Show)

data Pixel = MakePixel {coords :: Point, color :: Color} deriving (Show)


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

paint :: Functor f => Char -> f Point -> f Pixel
paint = fmap . flip MakePixel .  MakeColor

colorInThePoint :: HasCallStack => Point -> [Pixel] -> Color
colorInThePoint point pixels = color $ pixels !! i
    where
    i = fromMaybe (error "Elem doesn't exist") $
            elemIndex point (map coords pixels)


---------------------------- MONOCHROME SCREEN ---------------------------------
-- func monochromeScreen paints points turning them into pixels.
    -- Since it's monochrome, there is only one color to paint with
monochromeScreen :: [Point] -> [Pixel]
monochromeScreen pixelsToPaint = 
  [MakePixel p $ chooseMonochromeSymbol p
  | y <- screenYs
  , x <- screenXs
  , let p = MakePoint x y]
    where 
        chooseMonochromeSymbol coordPair
            | coordPair `elem` pixelsToPaint = defaultColor
            | otherwise = defaultBackgroundColor

monochromeFrame :: [Point] -> [String]
monochromeFrame pixelsToPaint = line <$> [0..screenHeight-1]
    where 
        line y =
            [symbol
            | pixel@(MakePixel p (MakeColor symbol))
                <- monochromeScreen pixelsToPaint
            , getX p == y
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
            (pt x) `elem` (map coords (rawColoredPixelsInLine y))
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
    filter $ \(coords -> pos) ->
        getX pos < screenWidth && getY pos < screenHeight

-- NEED TESTING!
frameMatrix :: [Pixel] -> [[Pixel]]
frameMatrix [] = [[]] --NEED FIX PROBABLY!
frameMatrix [pxN] = [[pxN]] --NEED FIX PROBABLY!
frameMatrix (px0:px1:pxs)
    | isLineContinues = (px0 : head (frameMatrix (px1:pxs))) : drop 1 (frameMatrix (px1:pxs)) --same line continued
    | otherwise = [px0] : frameMatrix (px1:pxs) --new line started
    where isLineContinues = getY (coords px0) == getY (coords px1)

pixelsLineToString :: [Pixel] -> String
pixelsLineToString = foldMap (pure . symbol . color)

matrixToString :: [[Pixel]] -> String
matrixToString pxsMatrix = unlines $ map pixelsLineToString pxsMatrix

frame01 :: [[Pixel]] -> String
frame01 figures = 
    matrixToString 
    . frameMatrix 
    . arrangePixels 
    . dropOutOfBoundsPixels 
    . concat 
    $ figures ++ [backgroundPixels]


