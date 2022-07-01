{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Painter
( Point (MakePoint)
, Color (MakeColor)
, frame01
, monochromeScreen
, monochromeFrame
, formPixelMatrix
, formStrings
) where
import Data.List.NonEmpty (groupBy, NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.Foldable (Foldable (toList))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow (first, Arrow ((&&&)))

-- * Types

data Point n = MakePoint {getX :: n, getY :: n} deriving (Show, Eq, Ord)

data Color a = MakeColor {symbol :: a} deriving (Show,Eq)

type Pixels n a = Map (Point n) (Color a)

data Screen coord color = MkScreen
    { screenWidth :: coord
    , screenHeight :: coord
    , defaultBackgroundColor :: Color color
    , defaultColor :: Color color
    , backgroundColor :: Color color
    }

screenXs :: (Enum n, Num n, Ord n) => Screen n c -> Set n
screenXs MkScreen {screenWidth} =
    Set.fromList [0 .. pred screenWidth]

screenYs :: (Enum n, Num n, Ord n) => Screen n c -> Set n
screenYs MkScreen {screenHeight} =
    Set.fromList [0 .. pred screenHeight]

-- * Monochrome screen

-- func monochromeScreen paints points turning them into pixels.
    -- Since it's monochrome, there is only one color to paint with
monochromeScreen :: forall n c.
    (Enum n, Num n, Ord n) =>
    Screen n c ->
    Set (Point n) ->
    Pixels n c
monochromeScreen
    screen@MkScreen {defaultColor,defaultBackgroundColor}
    pixelsToPaint =

    Map.fromSet chooseMonochromeSymbol (screenPoints screen)
    where
        chooseMonochromeSymbol :: Point n -> Color c
        chooseMonochromeSymbol coordPair
            | coordPair `elem` pixelsToPaint = defaultColor
            | otherwise = defaultBackgroundColor

monochromeFrame ::
    (Enum n, Num n, Ord n) =>
    Screen n c ->
    Set (Point n) ->
    [[Color c]]
monochromeFrame screen pixelsToPaint = line <$> toList (screenYs screen)
    where 
        line y =
            toList
            . Map.filterWithKey (const . (== y) . getY)
            $ monochromeScreen screen pixelsToPaint

-- * Multicolor

line' :: forall n c.
    (Enum n, Num n, Ord n) =>
    Screen n c ->
    n ->
    Pixels n c ->
    Pixels n c
line' screen@MkScreen{backgroundColor} y rawColoredPixels =
    Map.fromSet fill $ Set.map (`MakePoint` y) (screenXs screen) --ВЗЯЛ ЛИСТ В ЛИСТ И СОСАЛ ПОЛЧАСА
    where
        fill :: Point n -> Color c
        fill p = fromMaybe backgroundColor $
            Map.lookup p rawColoredPixelsInLine

        rawColoredPixelsInLine :: Pixels n c
        rawColoredPixelsInLine = --filtering by pixel.y
            Map.filterWithKey (const . (==) y . getY) rawColoredPixels

type Matrix n a = Map n (Map n a)

formPixelMatrix ::
    (Num n, Enum n, Ord n) =>
    Screen n c ->
    Pixels n c ->
    Matrix n (Color c)
formPixelMatrix screen rawColoredPixels = frameMatrix $
    foldMap (flip (line' screen) rawColoredPixels ) (screenYs screen)

formStrings :: Matrix n (Color c) -> [[Color c]]
formStrings pixelMatrix = 
    toList $ toList <$> pixelMatrix -- ДЫААААААА

{- | Turns nested maps into a flat map.
 It assumes to be O(n) since preserves order.
 Note: 'mergeKeys' MUST hold following rule:
 @(∀R ∈ {>,<,=,/=})((x,y) R (x1,y1) <=> mergeKeys x y R mergeKeys x1 y1)@
 -}
flattenMonotonic ::
    forall k0 k1 k a.
    Ord k =>
    (k0 -> k1 -> k) ->
    Map k0 (Map k1 a) ->
    Map k a
flattenMonotonic mergeKeys =
    Map.fromAscList -- Use 'Map.fromList' if 'mergeKeys' is not monotonic
    . mergeMaps
    . Map.assocs
    . fmap Map.assocs
    where 
        mergeMaps :: [(k0,[(k1,a)])] -> [(k,a)]
        mergeMaps = (uncurry fmap . first (first . mergeKeys) =<<)

{- | Turns flat maps into a nested maps.
 It assumes to be O(n) since preserves order.
 Note: 'splitKeys' MUST hold following rule:
 @(∀R ∈ {>,<,=,/=})(x R y <=> splitKeys x R splitKeys y)@
 -}
unFlattenMonotonic :: forall k0 k1 k a.
    (Ord k0, Ord k1) =>
    (k -> (k0,k1)) ->
    Map k a ->
    Map k0 (Map k1 a) 
unFlattenMonotonic splitKeys =
    fmap Map.fromAscList -- Use 'Map.fromList' if 'splitKeys' is not monotonic
    . Map.fromAscList -- Use 'Map.fromList' if 'splitKeys' is not monotonic
    . unmergeMaps
    . Map.assocs
    where 
        unmergeMaps ::  [(k,a)] -> [(k0,[(k1,a)])]
        unmergeMaps xs =
            fmap withGroup
            $ groupBy ((/=) `on` fst)
            $ pure . unmerge =<< xs
            
        withGroup :: NonEmpty (k0,(k1,a)) -> (k0,[(k1,a)])
        withGroup xs@((k,_) :| _) = (k,snd <$> toList xs)

        unmerge :: (k,a) -> (k0,(k1,a))
        unmerge (splitKeys -> (k0,k1),v) = (k0,(k1,v))

-- TODO: Put in docs
--------------------- 0.1.0 FRAME IMPLEMENTATION (with HOFs) -------------------
-- "frame" function alghorithm:
-- 1. get [figure] (figure = [Pixel]) 
    -- add backgroundPixels as a figure with the lowest precedence for its pixels
-- 2. form a matrix from that brushed set of pixels
-- 3. form one multiline string for the pixels matrix

screenPoints :: (Enum n, Num n, Ord n) => Screen n c -> Set (Point n)
screenPoints screen = Set.map (uncurry MakePoint) $
    Set.cartesianProduct (screenXs screen) (screenYs screen)

backgroundPixels :: (Enum n, Num n, Ord n) => Screen n c -> Pixels n c
backgroundPixels screen@MkScreen {backgroundColor} =
    Map.fromSet (const backgroundColor) (screenPoints screen)

isInBound :: Ord n => Screen n c -> Point n -> Bool
isInBound MkScreen {screenHeight, screenWidth} pos =
    getX pos < screenWidth && getY pos < screenHeight

dropOutOfBoundsPixels :: Ord n => Screen n c -> Pixels n c -> Pixels n c
dropOutOfBoundsPixels screen = Map.filterWithKey (const . isInBound screen)

frameMatrix :: Ord n => Pixels n c -> Matrix n (Color c)
frameMatrix = unFlattenMonotonic $ getX &&& getY

unMatrix :: Ord n => Matrix n (Color c) -> Pixels n c
unMatrix = flattenMonotonic MakePoint

-- * Concrete stuff

lineToString :: [Color Char] -> String
lineToString = foldMap (pure . symbol)

matrixToString :: Matrix n (Color Char) -> String
matrixToString = unlines . toList . fmap (lineToString . toList)

frame01 :: Matrix Int (Color Char) -> String
frame01 figures = 
    matrixToString 
    . frameMatrix
    . dropOutOfBoundsPixels defaultScreen
    $ Map.union (unMatrix figures) (backgroundPixels defaultScreen)

defaultScreen :: Screen Int Char
defaultScreen = MkScreen
    { screenWidth = 100
    , screenHeight = 11
    , defaultBackgroundColor = MakeColor '-'
    , defaultColor = MakeColor '@'
    , backgroundColor = MakeColor '.'
    }

myPixels :: Pixels Int Char
myPixels = Map.fromList
    [ pix 0 0 '#'
    , pix 0 1 'k'
    , pix 0 2 's'
    , pix 1 0 'G'
    , pix 1 2 'Q'
    ]
    where
        pix :: Int -> Int -> Char -> (Point Int,Color Char)
        pix x y sym = (MakePoint x y, MakeColor sym)

-- prettyPixel :: Pixel -> String
-- prettyPixel (MakePixel (MakePoint x y) (MakeColor color)) =
--     concat [show x, ":", show y, " ", [color]]

-- printMatrix :: [Pixel] -> IO ()
-- printMatrix = traverse_ (print . fmap prettyPixel) . frameMatrix
