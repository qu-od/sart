{-# OPTIONS_GHC -Weverything #-}
module Shapes (
    Shape (MakeShape),
    line,
    box,
    building,
    street,
) where

import Control.Arrow (first)
import Control.Monad (join)
import Data.Foldable (fold, maximumBy, minimumBy, toList)
import Data.Function (on)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Painter (
    Color (MakeColor),
    Pixels,
    Point (MakePoint, getX, getY),
 )

----------------- GENERALIZED VECTOR SHAPES (deprecated) -----------------------

-- shapes:
-- line (two points)
-- triangle (three points)
-- box (four points)
-- polyline (list of points)
-- axis-aligned box (two diagonal points)
-- axis-aligned polyline
-- circle (center and a radius)
-- sine (horizontal level, amplitude and freq)
-- 2d affine transform (turn transform and translation)
-- parametrize color for shapes

-------------------------------------- TYPES -----------------------------------
data Shape n = MakeShape [Point n] deriving (Show)

------------------------------------WHEEL FUNCS --------------------------------
ordRange :: (Ord a, Enum a) => a -> a -> [a]
ordRange x1 x2
    | x1 < x2 = [x1 .. x2]
    | otherwise = [x2 .. x1]

ordRangeSet :: (Ord a, Enum a) => a -> a -> Set a
ordRangeSet x1 x2
    | x1 < x2 = Set.fromDistinctAscList [x1 .. x2]
    | otherwise = Set.fromDistinctDescList [x2 .. x1]

-- | Makes a Set from ascending or descending distinct list
setFromDistinctMonotonicList :: Ord a => [a] -> Set a
setFromDistinctMonotonicList xs@(x0 : x1 : _)
    | x0 > x1 = Set.fromDistinctDescList xs
    | otherwise = Set.fromDistinctAscList xs
setFromDistinctMonotonicList xs = Set.fromList xs

------------------------------ SIMPLE (VERT-HORZ) SHAPES -----------------------
-- road :: Pixel -> Pixel -> [Pixel]
line :: (Ord n, Enum n) => Point n -> Point n -> Set (Point n)
-- types for other args arrangement
line (MakePoint x1 y1) (MakePoint x2 y2)
    | x1 == x2 = Set.mapMonotonic (MakePoint x1) $ ordRangeSet y1 y2 -- vertical road
    | y1 == y2 = Set.mapMonotonic (`MakePoint` y1) $ ordRangeSet x1 x2 -- horizontal road
    | otherwise =
        error
            "this road can be either \
            \ vertical or horizontal and not a diagonal"

-- road (x1, y1) len "left"  =
-- road (x1, y1) len "right" =
-- road (x1, y1) len "up"    =
-- road (x1, y1) len "down"  =

box :: (Ord n, Enum n) => Point n -> Point n -> Set (Point n)
-- types for other args arrangement
box (MakePoint x1 y1) (MakePoint x2 y2) =
    Set.mapMonotonic (uncurry MakePoint) $
        Set.cartesianProduct (ordRangeSet x1 x2) (ordRangeSet y1 y2)

-- box (x1, x2) width height =

--------------------------------- BUILDINGS ------------------------------------
buildingWallsColor :: Color Char
buildingWallsColor = MakeColor '#'

buildingBodyColor :: Color Char
buildingBodyColor = MakeColor ' '

repaintWithString :: Eq n => [b] -> Pixels n a -> Pixels n b
repaintWithString textToPaintWith =
    Map.fromAscList
        . zipWith (\b (pt, _) -> (pt, MakeColor b)) textToPaintWith
        . Map.toAscList

-- GUARD length textToPaintWith /= length points =
-- error "Len of points doesn't match with len of string to paint with"
-- GUARD otherwise = [MakePixel pt (MakeColor symbol) | ((MakePixel pt _), symbol) <- zip points textToPaintWith]

offsetShape :: Num n => Point n -> Pixels n c -> Pixels n c
offsetShape (MakePoint x0 y0) =
    Map.fromDistinctAscList . fmap (first offset) . Map.assocs
  where
    offset (MakePoint x y) = MakePoint (x + x0) (y + y0)

drawMatrixFrom :: Enum n => n -> [[Color a]] -> Pixels n a
drawMatrixFrom _ [] = Map.empty
drawMatrixFrom zero (firstRow : rows) =
    Map.fromDistinctAscList $
        join $
            zipWith
                ( \y row ->
                    zipWith
                        (\x elem -> (MakePoint x y, elem))
                        shapeXs
                        row
                )
                shapeYs
                rows
  where
    shapeXs = [zero .. len zero firstRow]
    shapeYs = [zero .. len zero rows]

len :: (Foldable f, Enum n) => n -> f a -> n
len zero xs = foldl (const . succ) zero xs

renderInCenter :: forall n c. Integral n => Pixels n c -> [[Color c]] -> Pixels n c
renderInCenter shape [] = shape
renderInCenter shape rawThing@(firstRow : _) =
    Map.union thing shape -- do not swap arguments!
  where
    thing = offsetShape offset $ drawMatrixFrom 0 rawThing

    offset =
        MakePoint
            { getX = start shapeSizeX thingSizeX
            , getY = start shapeSizeY thingSizeY
            }

    start :: forall n. Integral n => n -> n -> n
    start whole part = (whole - part) `div` 2

    shapeSizeX =
        let (upper, lower) = boundsBy getX shapePoints
         in abs $ upper - lower
    shapeSizeY =
        let (upper, lower) = boundsBy getY shapePoints
         in abs $ upper - lower
    shapePoints = Map.keys shape
    
    boundsBy f xs =
        ( f $ maximumBy (compare `on` f) xs
        , f $ minimumBy (compare `on` f) xs
        )
    thingSizeX = len 0 firstRow
    thingSizeY = len 0 rawThing

mkBuilding :: Integral n => Point n -> Point n -> String -> Pixels n Char
mkBuilding p0 p1 name =
    building buildingWallsColor buildingBodyColor p0 p1 (MakeColor <$> name)

building :: Integral n => Color c -> Color c -> Point n -> Point n -> [Color c] -> Pixels n c
building wallColor floorColor p1@(MakePoint x1 y1) p2@(MakePoint x2 y2) name =
    Map.union innerSpace walls
  where
    walls = Map.fromSet (const wallColor) outerSpacePoints
    innerSpace = flip renderInCenter [name] $
        Map.fromSet (const floorColor) innerSpacePoints

    outerSpacePoints = box p1 p2
    innerSpacePoints =
        let -- if a1 is right from a2 then reduce it, else increace
            decide a1 a2 = if a1 > a2 then pred a1 else succ a1
            p0' = MakePoint (decide x1 x2) (decide y1 y2)
            p1' = MakePoint (decide x2 x1) (decide y2 y1)
         in box p0' p1'

---------------------------------- STREETS -------------------------------------
streetColor :: Color Char
streetColor = MakeColor '+'

-- RENDER NAME IN THE CENTER NOT IN THE BEGINNING
street :: forall n. Integral n => Point n -> Point n -> String -> Pixels n Char
street p0 p1 rawName =
    renderInCenter rawStreet [name]
  where
    rawStreet = Map.fromSet (const streetColor) $ line p0 p1
    name = MakeColor <$> (" " ++ rawName ++ " st." ++ " ")
