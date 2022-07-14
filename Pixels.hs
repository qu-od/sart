{-# LANGUAGE ViewPatterns #-}

module Pixels
( IntPoint (MkIntPoint, iX, iY)
, GenColor (MkGenColor)
, GenPixel (MkGenPixel)
, xy
, leftX
, rightX
, upperY
, lowerY
, palette
, lookupColor
, colored
, toPairs'
, testPixels
, testPoints
) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

---------------------------------- TYPES ---------------------------------------


----------------------------- IntPoint type 0.1.1 ------------------------------
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


------------------------------ CONSTANTS ---------------------------------------
testPixels :: [GenPixel Char]
testPixels = 
    zipWith3 
    (\x y c -> MkGenPixel (MkIntPoint x y) (MkGenColor c))
    [3..] (repeat 3) "TEST PIXELS"

testPoints :: [IntPoint]
testPoints = map (uncurry MkIntPoint) [
    (136, 2),
    (149, 2),
    (149, 6),
    (10, 6),
    (10, 4),
    (134, 4),
    (134, 1) -- i=6
    ]

-------------------------- FUNCS -----------------------------------------------
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

lookupColor :: String -> GenColor Char
lookupColor string = 
    fromMaybe (error "No color in palette for that stuff you requested")
    (Map.lookup string palette)

colored :: String -> [IntPoint] -> [GenPixel Char]
colored colorKey = map (`MkGenPixel` lookupColor colorKey) --oh my