module Routes
(

) where

import Painter
    ( Point (MakePoint) -- deprecated
    , Color (MakeColor) -- deprecated
    , Pixel (MakePixel) -- deprecated
    )

-------------------------------- TYPES -----------------------------------------
-- data City = MakeCity {streets :: [Figure], stops :: [Point]} deriving (Show)

-- every turn of a Route MUST BE a stop
-- data Route = error "NOT IMPLEMENTED"
-- data Route = MakeRoute {number :: Int, turnPoints :: [Point]} deriving (Show)

------------------------------ IN-STREET ROUTES --------------------------------
-- CHECK CASES SYNTAX!
-- routePoints :: [Point] -> [Point]
-- routePoints [] = []
-- routePoints [ptN] = []
-- routePoints [ptNM1@(MakePoint xNM1 yNM1), ptN@(MakePoint xN yN)] = 
--    line (xNM1, yNM1) (xN, yN) -- no "init" since we want to add last point of the route
-- routePoints (pt0@(MakePoint x0 y0) : pt1@(MakePoint x1 y1) : points) = 
 --   init (line (x0, y0) (x1, y1)) ++ routePoints (pt1:points)

--addColorToThePointsShapeDecorator :: 
--addColorToThePointsShapeDecorator _ =

--route = addColorToThePointsShapeDecorator _ . routePoints --как вариант
 
--route :: Char -> [Point] -> Figure
--route symbol points = 
  --  MakeFigure [MakePixel pt (MakeColor symbol) | pt <- routePoints points]
    -------MakeFigure . (map (makePx symbol)) . routePoints points -- I tried...
    --where
      --  makePx symbol point = MakePixel point (MakeColor symbol)


------------------------------ FANCY ROUTES ------------------------------------
-- 1. make a list of streets and also coords of
    -- crossroads
    -- deadends
    -- and other busstops if needed
    -- make those streets and points into a data struct "CITY"
-- 2. construct route from a list of streets and other things from CITY
-- 3. brush a city in such a way that:
    -- routes are rendered not above streets but near or 1-2 block away from them
        --depending on how many routes are already lined up there in the street

