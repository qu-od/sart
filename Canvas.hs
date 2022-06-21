module Canvas
(frame
) where



--------------------------------- TYPES ----------------------------------------
data Point = MakePoint (Int, Int)
data Paint = MakePaint String -- ADD LEN=2 CONSTRAINT (or a literal constraint may be??)
data Pixel = MakePixel (Int, Int, Paint)

--------------------------------- CONSTS ---------------------------------------
lineLength :: Int
lineLength = 80 --default is 130
pictureHeight :: Int
pictureHeight = 6 --default is 15
emptyFieldSymbol :: String
emptyFieldSymbol = "--"
paintedFieldSymbol :: String
paintedFieldSymbol = "@@"
screenSize :: (Int, Int)
screenSize = (lineLength, pictureHeight) -- i.e. screen_width and screen_height

------------------------------ CANVAS CONFIG -----------------------------------
screen :: [(Int, Int)] -> [(Int, Int, String)]
screen = monochromeScreen

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

--------------------------------- SCREENS --------------------------------------
-- data struct: Pixel = (pixel_x, pixel_y, pixel_symbol)
-- monochromeScreen :: [Point] -> [Pixel]
-- func monochromeScreen paints points turning them into pixels.
    -- Since it's monochrome, there is only one color to paint with
monochromeScreen :: [(Int, Int)] -> [(Int, Int, String)]
monochromeScreen pixelsToPaint = 
  [(x, y, chooseMonochromeSymbol (x, y)) | y <- ys, x <- xs]
    where 
        xs = [0 .. lineLength-1]
        ys = [0 .. pictureHeight-1]
        chooseMonochromeSymbol coordPair = if coordPair `elem` pixelsToPaint --
            then paintedFieldSymbol
            else emptyFieldSymbol


--------------------------- FRAME FROM LINES -----------------------------------
frame :: [(Int, Int)] -> [String]
frame pixelsToPaint = [line y | y <- [0..pictureHeight-1]]
    where line y = concat [
            thrd pixel | pixel <- screen pixelsToPaint, scnd pixel == y
            ]





