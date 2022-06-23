module Painter
( Point (MakePoint)
, Color (MakeColor)
, Pixel (MakePixel)
, frame
, paint
) where


--------------------------------- TYPES ----------------------------------------
data Point = MakePoint {getX :: Int, getY :: Int} deriving (Show, Eq)

data Color = MakeColor {symbol :: Char} deriving (Show)

data Pixel = MakePixel {coords :: Point, color :: Color} deriving (Show)


--------------------------------- CONSTS ---------------------------------------
screenWidth :: Int
screenWidth = 180

screenHeight :: Int
screenHeight = 6

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

frame :: [[Pixel]] -> String
frame figures = unlines $ formStrings $ formPixelMatrix $ concat figures





