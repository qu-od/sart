module Shapes
( Shape (MakeShape)
, road
, box
) where

import Painter
    (Point (MakePoint)
    )
-----------------------GENERALIZED VECTOR SHAPES -------------------------------

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
data Shape = MakeShape [Point] deriving (Show)

------------------------------ SIMPLE (VERT-HORZ) SHAPES -----------------------
-- road :: Pixel -> Pixel -> [Pixel]
road :: (Int, Int) -> (Int, Int) -> [Point]
-- types for other args arrangement
road (x1, y1) (x2, y2)
    | x1 == x2 = [MakePoint x1 y | y <- [y1 .. y2]] --vertical road
    | y1 == y2 = [MakePoint x y1 | x <- [x1 .. x2]] --horizontal road
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
