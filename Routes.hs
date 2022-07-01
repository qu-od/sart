module Routes
( Route (MakeRoute)
, routePoints
) where

import Painter
    ( Point (MakePoint)
    , Color (MakeColor)
    )

import Shapes
    (line
    )
import Data.Set (Set)
import Data.Foldable (toList)
import qualified Data.Set as Set

-------------------------------- TYPES -----------------------------------------
data Route n = MakeRoute {number :: Int, turnPoints :: [Point n]} deriving (Show)

withSetAscDesc :: ([a] -> [b]) -> Set a -> Set b
withSetAscDesc f = Set.fromDistinctAscList . f . toList
------------------------------ IN-STREET ROUTES --------------------------------
-- CHECK CASES SYNTAX!
routePoints :: (Ord n, Enum n) => Set (Point n) -> Set (Point n)
routePoints (toList -> xs) =
    case xs of 
    [] -> Set.empty
    [ptN] -> Set.empty
    [ptNM1,ptN] -> line ptNM1 ptN -- no "init" since we want to add last point of the route
    (pt0 : pt1 : points) -> 
        withSetAscDesc init (line pt0 pt1)
        `Set.union` routePoints (Set.fromDistinctAscList $ pt1:points)

--addColorToThePointsShapeDecorator :: 
--addColorToThePointsShapeDecorator _ =

--route :: 
--route = addColorToThePointsShapeDecorator _ . routePoints


------------------------------ FANCY ROUTES ------------------------------------
