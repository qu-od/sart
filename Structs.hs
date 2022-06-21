module Structs
( screenSize
, mCScreen
--, Point, Paint, Pixel
) where

--------------------------------- TYPES ----------------------------------------
data Point = MakePoint (Int, Int)
data Paint = MakePaint String -- ADD LEN=2 CONSTRAINT (or a literal constraint may be??)
data Pixel = MakePixel (Int, Int, Paint)

--------------------------------- CONSTS ---------------------------------------
lineLength :: Int
lineLength = 50 --default is 130
pictureHeight :: Int
pictureHeight = 10 --default is 15
emptyFieldSymbol :: String
emptyFieldSymbol = "--"
paintedFieldSymbol :: String
paintedFieldSymbol = "@@"
screenSize :: (Int, Int)
screenSize = (lineLength, pictureHeight) -- i.e. screen_width and screen_height

--------------------------------- FUNCS ----------------------------------------
-- data struct: Pixel = (pixel_x, pixel_y, pixel_symbol)
-- MCScreen stands for MonoChromeScreen
mCScreen :: [(Int, Int)] -> [(Int, Int, String)]
mCScreen coordsOfPixelsToPaint = [(x, y, appropSymbolMCScreen (x, y)) | y <- ys, x <- xs]
    where 
        xs = [0 .. lineLength-1]
        ys = [0 .. pictureHeight-1]
        appropSymbolMCScreen coordPair = if coordPair `elem` coordsOfPixelsToPaint
            then paintedFieldSymbol
            else emptyFieldSymbol


