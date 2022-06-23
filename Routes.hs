module Routes
( Route (MakeRoute)
, routePoints
) where

import Painter
    ( Point (MakePoint)
    , Color (MakeColor)
    , Pixel (MakePixel)
    )

import Shapes
    (line
    )

-------------------------------- TYPES -----------------------------------------
data Route = MakeRoute {number :: Int, turnPoints :: [Point]} deriving (Show)


------------------------------ IN-STREET ROUTES --------------------------------
-- CHECK CASES SYNTAX!
routePoints :: [Point] -> [Point]
routePoints [] = []
routePoints [ptN] = []
routePoints [ptNM1@(MakePoint xNM1 yNM1), ptN@(MakePoint xN yN)] = 
    line (xNM1, yNM1) (xN, yN) -- no "init" since we want to add last point of the route
routePoints (pt0@(MakePoint x0 y0) : pt1@(MakePoint x1 y1) : points) = 
    init (line (x0, y0) (x1, y1)) ++ routePoints (pt1:points)

--addColorToThePointsShapeDecorator :: 
--addColorToThePointsShapeDecorator _ =

--route :: 
--route = addColorToThePointsShapeDecorator _ . routePoints


------------------------------ FANCY ROUTES ------------------------------------
