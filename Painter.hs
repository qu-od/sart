module Painter
(frame
, paint
) where


--------------------------------- TYPES ----------------------------------------
data Point = MakePoint (Int, Int)

data Paint = MakePaint String -- ADD LEN=2 CONSTRAINT (or a literal constraint may be??)

data Pixel = MakePixel (Int, Int, Paint)


--------------------------------- CONSTS ---------------------------------------
screenWidth :: Int
screenWidth = 80

screenHeight :: Int
screenHeight = 6

backgroundColorSymbol :: Char
backgroundColorSymbol = '.'

defaultBackgroundColorSymbol :: Char
defaultBackgroundColorSymbol = '-'

defaultColorSymbol :: Char
defaultColorSymbol = '@'

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

-- Point -> Char -> Pixel
paint :: Char -> [(Int, Int)] -> [(Int, Int, String)]
paint symbol points = [(x, y, [symbol, symbol]) | (x, y) <- points]

pixelsCoords :: [(Int, Int, String)] -> [(Int, Int)]
pixelsCoords pixels = [(x, y) | (x, y, _) <- pixels]

colorOfPoint :: (Int, Int) -> [(Int, Int, String)] -> String
colorOfPoint point pixels = thrd (pixels !! i)
    where i = findIndexOfTheFirstMatch point (pixelsCoords pixels)


---------------------------- MONOCHROME SCREEN ---------------------------------
-- data struct: Pixel = (pixel_x, pixel_y, pixel_symbol)
-- monochromeScreen :: [Point] -> [Pixel]
-- func monochromeScreen paints points turning them into pixels.
    -- Since it's monochrome, there is only one color to paint with
monochromeScreen :: [(Int, Int)] -> [(Int, Int, String)]
monochromeScreen pixelsToPaint = 
  [(x, y, chooseMonochromeSymbol (x, y)) | y <- ys, x <- xs]
    where 
        xs = [0 .. screenWidth-1 ]
        ys = [0 .. screenHeight-1]
        chooseMonochromeSymbol coordPair = if coordPair `elem` pixelsToPaint --
            then replicate 2 defaultColorSymbol
            else replicate 2 defaultBackgroundColorSymbol

monochromeFrame :: [(Int, Int)] -> [String]
monochromeFrame pixelsToPaint = [line y | y <- [0..screenHeight-1]]
    where line y = concat [
            thrd pixel | pixel <- monochromeScreen pixelsToPaint, scnd pixel == y
            ]


----------------------- MULTICOLOR IMPLEMENTATION ------------------------------
line' :: Int -> [(Int, Int, String)] -> [(Int, Int, String)]
line' lineY rawColoredPixels = [
    if pixelWasColored x then symbolFromColoredPixel x else symbolFromBackgroundPixel x | x <- screenXs --ВЗЯЛ ЛИСТ В ЛИСТ И СОСАЛ ПОЛЧАСА
    ]
    where
        rawColoredPixelsInLine lineY = --filtering by pixel.y
            [(x, y, color) | (x, y, color) <- rawColoredPixels, y == lineY]
        pixelWasColored x = 
            (x, lineY) `elem` pixelsCoords (rawColoredPixelsInLine lineY)
        symbolFromColoredPixel x =
            (x, lineY, colorOfPoint (x, lineY) (rawColoredPixelsInLine lineY))
        symbolFromBackgroundPixel x =
            (x, lineY, replicate 2 backgroundColorSymbol)
        
-- sort by line. Then sort by x_pos in line and remove duplicates
formPixelMatrix :: [(Int, Int, String)] -> [[(Int, Int, String)]]
formPixelMatrix rawColoredPixels = [line' y rawColoredPixels | y <- screenYs]
        -- first pixel that matches (x0, y0) coords is chosen.
        -- So pixel in (x0, y0) will be painted in correspondent color

formStrings :: [[(Int, Int, String)]] -> [String]
formStrings pixelmatrix = [concat $ symbols pixelLine | pixelLine <- pixelmatrix]
    where symbols pixels = [symbol | (_, _, symbol) <- pixels]

frame :: [[(Int, Int, String)]] -> String
frame figures = unlines $ formStrings $ formPixelMatrix $ concat figures





