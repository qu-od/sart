{-# LANGUAGE ViewPatterns #-}

module Busstops
( Busstop (
    Intersection,
    Deadend,
    Extra
    )
, testBusstops
) where

import Pixels
    ( IntPoint (MkIntPoint, iX, iY)
    , GenColor (MkGenColor)
    , GenPixel (MkGenPixel)
    , xy
    )

import Shapes
    ( Shape (
        Building,
        Street, stName, pt1, pt2
        )
    , testShapes
    , streetAxis
    , interpolate'
    , isStreet
    , streetRequiredError
    )


-------------------------------- TYPES -----------------------------------------
data Busstop =
    -- record syntax DISCOURAGED when multiple value constructors are used
    Intersection {
        stOX :: Shape,
        stOY :: Shape,
        iPt :: IntPoint
        } 
    | Deadend { -- every "start" and every "end" of the street 
        sDs :: Shape,
        dPt :: IntPoint
        }
    | Extra { -- in the middle of the street --BAD NAMING NEPONYATNO NIHUIA
        eSt :: Shape,
        ePt :: IntPoint
        }

instance Show Busstop where
    show (Intersection st1 st2 pt) = concat [
        "Place \"Intersection\" of ",
        stName st1, " st. and ",
        stName st2, " st. at",
        xy pt
        ]
    show (Deadend st pt) =
        "Place \"Deadend\" of " ++ stName st ++ " st. at " ++ xy pt
    show (Extra st pt) = 
        "Place \"ExtraBusstop\" on a " ++ stName st ++ " st. at " ++ xy pt




-------------------------------- CONSTS ----------------------------------------



---------------------- BUSSTOPS GENERATION (for frame014) ----------------------
-- USE SETS THERE (to dumb too slow, so I won't)
-- |Filter out vertical lines
-- |Filter out horizontal lines
-- |For each horizontal line find intersection with
-- |Could be optimized with predicates that'll tell if intersection between
-- -- |two streets is even possible given their endpoints
intersectionsOfStreets :: [Shape] -> [Busstop]
intersectionsOfStreets sts = [ --part. app. of map
    Intersection stOX stOY (MkIntPoint (iX (pt1 stOY)) (iY (pt1 stOX))) |
    stOX <- stsOX sts, stOY <- stsOY sts,
    stOX `shouldIntersect` stOY 
    ]
    where
        stsOX = filter ((== "oX") . streetAxis)
        stsOY = filter ((== "oY") . streetAxis)
        xAligned (Street _ st1p1 st1p2) (Street _ st2p1 st2p2) =
            (iX st1p1 <= iX st2p1) && (iX st2p1 <= iX st1p2)
        xAligned _ _ = error "StreetPP required"
        yAligned (Street _ st1p1 st1p2) (Street _ st2p1 st2p2) =
            (iY st2p1 <= iY st1p1) && (iY st1p1 <= iY st2p2)
        yAligned _ _ = error "StreetPP required"
        stX `shouldIntersect` stY = xAligned stX stY && yAligned stX stY
    

-- |should we make it return a pair instead of a list to 
-- -- |highlight the fact that there are 2 deadends?
deadendsOfStreet :: Shape -> [Busstop]
deadendsOfStreet st@(Street _ pt1 pt2) = [
    Deadend st pt1, Deadend st pt2
    ]
deadendsOfStreet _ = error "StreetPP required"

-- |how extra busstops are added.. (whit papir)
-- |let distance between busstops be D (defaultBusstopDistance :: Int)
-- |then there are D-1 empty street points between them
-- |every deadend and an intersection is a busstop by default
-- -- |and they'll be finded by other functions
-- |busstops are added from the top or from the left
-- -- |(depends on street direction)
-- |and let's be generous not to delete last busstop just before pt2
-- -- |only because distance between them could be less than D
-- |So extra busstops will be having following indices among street points: 
-- -- |Xi=[D, 2*D, 3*D, ..., N*D] where Xi <= streetLen - 2
-- |...Nevermind, let's just use
defaultBusstopDistance :: Int
defaultBusstopDistance = 5
        
-- using (mod D == 0) predicate
-- already assuming that is a street indeed
extraBusstopsForStreet :: Shape -> [Busstop]
extraBusstopsForStreet st = map (Extra st) (filter (isBusstop st) (interpolate' st))
    where
        d = defaultBusstopDistance
        isBusstop st p = if streetAxis st == "oX"
            then (iX p `mod` d) == 0
            else (iY p `mod` d) == 0
                            
testBusstops :: [Busstop]
testBusstops = concat [
    intersectionsOfStreets streets,
    concatMap deadendsOfStreet streets, 
    concatMap extraBusstopsForStreet streets -- concatMap OH MY!
    ]
    where
        streets = filter isStreet testShapes

testMiscPixels :: [GenPixel Char]
testMiscPixels = []


