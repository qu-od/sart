module Shapes
( road
, box
) where

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

------------------------------ SIMPLE (VERT-HORZ) SHAPES -------------------------
-- raad :: Pixel -> Pixel -> [Pixel]
road :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
-- types for other args arrangement
road (x1, y1) (x2, y2)
    | x1 == x2 = [(x1, y) | y <- [y1 .. y2]] --vertical road
    | y1 == y2 = [(x, y1) | x <- [x1 .. x2]] --horizontal road
    | otherwise = error "this road can be either \
        \ vertical or horizontal and not a diagonal"
-- road (x1, y1) len "left"  = 
-- road (x1, y1) len "right" = 
-- road (x1, y1) len "up"    = 
-- road (x1, y1) len "down"  = 

box :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
-- types for other args arrangement
box (x1, y1) (x2, y2) = 
    [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]
-- box (x1, x2) width height = 
