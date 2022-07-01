{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
module Painter
( Point (MakePoint)
, Color (MakeColor)
, frame01
) where
import Data.List (elemIndex, groupBy)
import GHC.Stack (HasCallStack)
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.Foldable (traverse_, Foldable (toList))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (join, forM, when)
import Control.Arrow (first, Arrow ((&&&)))
import qualified GHC.Arr as STArr
import qualified Control.Monad.ST as ST
import Control.Monad.Fix
import Data.Monoid (First(First,getFirst))
--------------------------------- TYPES ----------------------------------------
data Point = MakePoint {getX :: Int, getY :: Int} deriving (Show, Eq, Ord)

data Color = MakeColor {symbol :: Char} deriving (Show,Eq)

type Pixels = Map Point Color

--------------------------------- CONSTS ---------------------------------------
screenWidth :: Int
screenWidth = 100

screenHeight :: Int
screenHeight = 11

backgroundColor :: Color
backgroundColor = MakeColor '.'

defaultBackgroundColor :: Color
defaultBackgroundColor = MakeColor '-'

defaultColor :: Color
defaultColor = MakeColor '@'

screenSize :: (Int, Int)
screenSize = (screenWidth, screenHeight)

screenXs :: Set Int
screenXs = Set.fromList [0 .. screenWidth - 1]

screenYs :: Set Int
screenYs = Set.fromList [0 .. screenHeight - 1]


--------------------------------- WHEELS ---------------------------------------
scnd :: (a, b, c) -> b
scnd (_, y, _) = y

thrd :: (a, b, c) -> c
thrd (_, _, z) = z

colorInThePoint :: HasCallStack => Point -> Pixels -> Color
colorInThePoint = (unwrap .) . Map.lookup
    where
    unwrap = fromMaybe (error "Elem doesn't exist")

---------------------------- MONOCHROME SCREEN ---------------------------------
-- func monochromeScreen paints points turning them into pixels.
    -- Since it's monochrome, there is only one color to paint with
monochromeScreen :: Set Point -> Pixels
monochromeScreen pixelsToPaint =
    Map.fromSet chooseMonochromeSymbol screenPoints
    where 
        chooseMonochromeSymbol coordPair
            | coordPair `elem` pixelsToPaint = defaultColor
            | otherwise = defaultBackgroundColor

monochromeFrame :: Set Point -> [String]
monochromeFrame pixelsToPaint = line <$> toList screenYs 
    where 
        line y =
            toList
            . Map.map symbol
            . Map.filterWithKey (const . (== y) . getY)
            $ monochromeScreen pixelsToPaint

----------------------- MULTICOLOR IMPLEMENTATION ------------------------------
line' :: Int -> Pixels -> Pixels
line' y rawColoredPixels =
    Map.fromSet fill $ Set.map (`MakePoint` y) screenXs --ВЗЯЛ ЛИСТ В ЛИСТ И СОСАЛ ПОЛЧАСА
    where
        fill :: Point -> Color
        fill x
            | isColored x = colored x
            | otherwise = background x

        rawColoredPixelsInLine :: Int -> Pixels
        rawColoredPixelsInLine y = --filtering by pixel.y
            Map.filterWithKey (const . (==) y . getY) rawColoredPixels
        
        isColored = (`Map.member` rawColoredPixelsInLine y)
        colored p = colorInThePoint p (rawColoredPixelsInLine y)
        background _ = backgroundColor

type Matrix n a = Map n (Map n a)

formPixelMatrix :: Pixels -> Matrix Int Color
formPixelMatrix rawColoredPixels = frameMatrix $
    foldMap (`line'` rawColoredPixels) screenYs

formStrings :: Matrix Int Color -> [String]
formStrings pixelMatrix = 
    toList $ toList . fmap symbol <$> pixelMatrix -- ДЫААААААА

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
            
        withGroup :: [(k0,(k1,a))] -> (k0,[(k1,a)])
        withGroup xs@((k,_):_) = (k,snd <$> xs)

        unmerge :: (k,a) -> (k0,(k1,a))
        unmerge (splitKeys -> (k0,k1),v) = (k0,(k1,v))

--------------------- 0.1.0 FRAME IMPLEMENTATION (with HOFs) -------------------
-- "frame" function alghorithm:
-- 1. get [figure] (figure = [Pixel]) 
    -- add backgroundPixels as a figure with the lowest precedence for its pixels
-- 2. form a matrix from that brushed set of pixels
-- 3. form one multiline string for the pixels matrix

screenPoints :: Set Point
screenPoints = Set.map (uncurry MakePoint) $
    Set.cartesianProduct screenXs screenYs

backgroundPixels :: Pixels
backgroundPixels = Map.fromSet (const backgroundColor) screenPoints

isInBound :: Point -> Bool
isInBound pos = getX pos < screenWidth && getY pos < screenHeight

dropOutOfBoundsPixels :: Pixels -> Pixels
dropOutOfBoundsPixels = Map.filterWithKey (const . isInBound)

frameMatrix :: Pixels -> Matrix Int Color
frameMatrix = unFlattenMonotonic $ getX &&& getY

unMatrix :: Matrix Int Color -> Pixels
unMatrix = flattenMonotonic MakePoint

lineToString :: [Color] -> String
lineToString = foldMap (pure . symbol)

matrixToString :: Matrix Int Color -> String
matrixToString = unlines . toList . fmap (lineToString . toList)

frame01 :: Matrix Int Color -> String
frame01 figures = 
    matrixToString 
    . frameMatrix
    . dropOutOfBoundsPixels 
    $ Map.union (unMatrix figures) backgroundPixels


myPixels :: Pixels
myPixels = Map.fromList
    [ pix 0 0 '#'
    , pix 0 1 'k'
    , pix 0 2 's'
    , pix 1 0 'G'
    , pix 1 2 'Q'
    ]
    where
        pix :: Int -> Int -> Char -> (Point,Color)
        pix x y sym = (MakePoint x y, MakeColor sym)

-- prettyPixel :: Pixel -> String
-- prettyPixel (MakePixel (MakePoint x y) (MakeColor color)) =
--     concat [show x, ":", show y, " ", [color]]

-- printMatrix :: [Pixel] -> IO ()
-- printMatrix = traverse_ (print . fmap prettyPixel) . frameMatrix
