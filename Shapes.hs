module Shapes
( Shape (MakeShape)
, line
, box
, building
, street
) where

import Painter
    ( Point (MakePoint)
    , Color (MakeColor)
    , Pixel (MakePixel)
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
    | x1 < x2 = [x1..x2]
    | otherwise = [x2..x1]


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

box :: (Int, Int) -> (Int, Int) -> [Point]
-- types for other args arrangement
box (x1, y1) (x2, y2) = 
    [MakePoint x y | x <- [x1 .. x2], y <- [y1 .. y2]]
-- box (x1, x2) width height = 

--------------------------------- BUILDINGS ------------------------------------
buildingWallsColor :: Color
buildingWallsColor = MakeColor '#'

buildingBodyColor :: Color
buildingBodyColor = MakeColor ' '

repaintWithString :: String -> [Pixel] -> [Pixel]
repaintWithString textToPaintWith points = 
    [MakePixel pt (MakeColor symbol) | (MakePixel pt _, symbol) <- zip points textToPaintWith]
    -- GUARD length textToPaintWith /= length points =
       -- error "Len of points doesn't match with len of string to paint with"
    -- GUARD otherwise = [MakePixel pt (MakeColor symbol) | ((MakePixel pt _), symbol) <- zip points textToPaintWith]

--RENDER NAME IN THE CENTER NOT IN THE CORNER
building :: Point -> Point -> String -> [Pixel] -- map composed func to every (x, y)
building (MakePoint x1 y1) (MakePoint x2 y2) name = 
    map (paintWall . point) (concat [
        [(x, min y1 y2) | x <- xs], -- upper wall --Запятые забыл добавить. Тоже пососал знатно
        [(x, max y1 y2) | x <- xs], -- lower wall
        [(min x1 x2, y) | y <- choppedYs], -- left wall
        [(max x1 x2, y) | y <- choppedYs] -- right wall
    ]) ++ 
    addNameToBody [(paintBody . point) (x, y) | y <- choppedYs, x <- choppedXs] -- inner points
    where
        len = length
        xs = ordRange x1 x2
        ys = ordRange y1 y2
        choppedXs = (init . tail) xs
        choppedYs = (init . tail) ys
        point (x, y) = MakePoint x y
        --nameStartX xs = (middle xs) - (middle name) --TOOMUCH FOR TODAY
        --nameStopX xs  = (middle xs) + (middle name)
        --nameY ys = middle ys
        --repaintStartIndexInMatrix = 
        paintWall wallPoint = MakePixel wallPoint buildingWallsColor
        paintBody bodyPoint = MakePixel bodyPoint buildingBodyColor
        nameIsRendered = if (len ys >= 3) && (len xs >= (len name) + 2)
            then True else False
        addNameToBody pixels = if nameIsRendered
            then (repaintWithString name (take (len name) pixels)) ++ drop (len name) pixels 
            else pixels


---------------------------------- STREETS -------------------------------------
streetColor :: Color
streetColor = MakeColor '+'

middle :: [a] -> Int
middle xs = (length xs) `div` 2

--RENDER NAME IN THE CENTER NOT IN THE BEGINNING
street :: Point -> Point -> String -> [Pixel]
street (MakePoint x1 y1) (MakePoint x2 y2) rawName
    | x1 == x2 = tryToRenderName [MakePixel (MakePoint x1 y) streetColor | y <- ordRange y1 y2] -- vertical road
    | y1 == y2 = tryToRenderName [MakePixel (MakePoint x y1) streetColor | x <- ordRange x1 x2] -- horizontal road
    | otherwise = error "this road can be either \
        \ vertical or horizontal and not a diagonal"
    where
        name = rawName ++ " st."
        repaintStartIndex pixels = (middle pixels) - (middle name)
        repaintStopIndex pixels  = (middle pixels) + (middle name)        
        tryToRenderName pixels = if (length pixels >= (length name) + 4)
            then concat [ --OPTIMIZE DEFINITION!
                take (repaintStartIndex pixels) pixels,
                repaintWithString (" " ++ name ++ " ") (drop (repaintStartIndex pixels) (take ((repaintStopIndex pixels) + 3) pixels)),
                drop ((repaintStopIndex pixels) + 3) pixels
                ]
            else pixels
    

