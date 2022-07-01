module Shapes
( Shape (MakeShape)
, line
, box
, building
, street
) where
import Data.Map qualified as Map
import Painter
    ( Point (MakePoint)
    , Color (MakeColor)
    , Pixels
    )
import Data.Set (Set)
import qualified Data.Set as Set
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
    | x1 < x2 = [x1..x2]
    | otherwise = [x2..x1]

ordRangeSet :: (Ord a, Enum a) => a -> a -> Set a
ordRangeSet x1 x2
    | x1 < x2 = Set.fromDistinctAscList [x1..x2]
    | otherwise = Set.fromDistinctDescList [x2..x1]

-- | Makes a Set from ascending or descending distinct list 
setFromDistinctMonotonicList :: Ord a => [a] -> Set a
setFromDistinctMonotonicList xs@(x0:x1:_)
    | x0 > x1 = Set.fromDistinctDescList xs
    | otherwise = Set.fromDistinctAscList xs
setFromDistinctMonotonicList xs = Set.fromList xs

------------------------------ SIMPLE (VERT-HORZ) SHAPES -----------------------
-- road :: Pixel -> Pixel -> [Pixel]
line :: (Ord n, Enum n) => (n, n) -> (n, n) -> Set (Point n)
-- types for other args arrangement
line (x1, y1) (x2, y2)
    | x1 == x2 = Set.mapMonotonic (MakePoint x1) $ ordRangeSet y1 y2 --vertical road
    | y1 == y2 = Set.mapMonotonic (`MakePoint` y1) $ ordRangeSet x1 x2 --horizontal road
    | otherwise = error "this road can be either \
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
    . zipWith (\b (pt,_) -> (pt,MakeColor b)) textToPaintWith
    . Map.toAscList 
    -- GUARD length textToPaintWith /= length points =
       -- error "Len of points doesn't match with len of string to paint with"
    -- GUARD otherwise = [MakePixel pt (MakeColor symbol) | ((MakePixel pt _), symbol) <- zip points textToPaintWith]


len :: (Foldable f, Enum n) => n -> f a -> n
len zero xs = foldl (const . succ) zero xs

mkBuilding :: Integral n => Point n -> Point n -> String -> Pixels n Char
mkBuilding p0 p1 name =
    building buildingWallsColor buildingBodyColor p0 p1 (MakeColor <$> name)

building :: Integral n => Color c -> Color c -> Point n -> Point n -> [Color c] -> Pixels n c
building wallColor floorColor p1@(MakePoint x1 y1) p2@(MakePoint x2 y2) name =
    Map.union walls innerSpace
    where
        walls = flip renderInCenter [name] $
            Map.fromSet (const wallColor) outerSpacePoints
        innerSpace = Map.fromSet (const floorColor) innerSpacePoints

        outerSpacePoints = box p1 p2
        innerSpacePoints =
            let -- | if a1 is right from a2 then reduce it, else increace
                decide a1 a2 = if a1 > a2 then pred a1 else succ a1
                p0' = MakePoint (decide x1 x2) (decide y1 y2)
                p1' = MakePoint (decide x2 x1) (decide y2 y1) 
            in box p0' p1'

---------------------------------- STREETS -------------------------------------
streetColor :: Color Char
streetColor = MakeColor '+'

middle :: [a] -> Int
middle xs = (length xs) `div` 2

--RENDER NAME IN THE CENTER NOT IN THE BEGINNING
street :: forall n. (Ord n, Enum n) => Point n -> Point n -> String -> Pixels n Char
street (MakePoint x1 y1) (MakePoint x2 y2) rawName = renderResult
    where
        rawStreet = line (x1,y1) (x2,y2)
        name = " " ++ rawName ++ " st." ++ " "
        offset = (length rawStreet - length name) `div` 2

        repaintStartIndex = offset
        repaintStopIndex = offset + length name

        pixels = Map.fromSet (const streetColor) rawStreet

        renderResult :: Pixels n Char    
        renderResult
            | length pixels > length name =
                let (prefix,(target,postfix)) =
                        splitAt repaintStopIndex
                        <$> splitAt repaintStartIndex (Map.assocs pixels)
                in fold 
                    [ Map.fromDistinctAscList prefix
                    , repaintWithString name $ Map.fromDistinctAscList target
                    , Map.fromDistinctAscList postfix
                    ]
            | otherwise = pixels
    

