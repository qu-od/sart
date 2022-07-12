{-# LANGUAGE ViewPatterns #-}

module Shapes  -- Module also includes makinng Busstops and "other Pixels"
( testShapes
, testBusstops
, testMiscPixels
, testPoints
) where

import Painter
    ( IntPoint (MkIntPoint, iX, iY) --test getters import
    , GenColor (MkGenColor)
    , GenPixel (MkGenPixel)
    , Shape 
        ( EmptyShape
        , Building
        , StreetPP, pt1, pt2
        , Route )
    , ensureStreetPP
    , streetAxis
    , interpolate'
    , isStreet
    , Busstop 
        ( Intersection
        , Deadend
        , Extra)
    )








----------------------------- TEST DATA ----------------------------------------
testPoint :: IntPoint
testPoint = MkIntPoint 2 3

testCharGenColor :: GenColor Char
testCharGenColor = MkGenColor '$'

testGenPixel :: GenPixel Char
testGenPixel = MkGenPixel testPoint testCharGenColor

p :: Int -> Int -> IntPoint
p = MkIntPoint

testPoints :: [IntPoint]
testPoints = map (uncurry p) [
    (136, 2),
    (149, 2),
    (149, 6),
    (10, 6),
    (10, 4),
    (134, 4),
    (134, 1) -- i=6
    ]

testShapes :: [Shape]
testShapes = [ -- leading elements have higher priority in rendering
    Building "TEST BUILDING 1"  (p  20  0) (p  42  4),
    Building "5букв"            (p  50  1) (p  56  4),
    Building "TEST BUILdinG 68" (p  40  4) (p  60  6),
    Building " в ебенях"        (p  44 14) (p  70 19),
    Building "что-то"           (p  74  4) (p  80  6),
    StreetPP "foo"              (p 150  5) (p 100  5),
    StreetPP "Yablochkova"      (p 150  3) (p 170  3),
    StreetPP "Kosmonavtov"      (p  50  5) (p  50  0),
    StreetPP "Gayorgyeva"       (p 150  3) (p 120  3),
    StreetPP "Svobody"          (p 100  5) (p 150  5),
    StreetPP "Vo"               (p 120  0) (p 120  5),
    StreetPP "TUPIKOVYI TYPIK"  (p 150  2) (p 399  2),
    StreetPP "Бельфегоровская"  (p 171  2) (p 228  2),
    StreetPP "test"             (p   6  8) (p 103  8),
    StreetPP "test 2"           (p  72  0) (p  72 13),
    --
    Route 1 (zipWith p [40, 44, 44]        [4, 4, 14]),
    Route 2 (zipWith p [136, 149, 149, 10] [2, 2, 6, 6]),
    Route 3 (zipWith p [10, 10, 134, 134]  [6, 4, 4, 1])
    ]



----------------------------- BUSSTOPS GENERATION ------------------------------
-- USE SETS THERE (to dumb too slow, so I won't)
-- |Filter out vertical lines
-- |Filter out horizontal lines
-- |For each horizontal line find intersection with
-- |Could be optimized with predicates that'll tell if intersection between
-- -- |two streets is even possible given their endpoints
intersectionsOfStreets :: [Shape] -> [Busstop]
intersectionsOfStreets (map ensureStreetPP -> sts) = [ --part. app. of map
    Intersection stOX stOY (MkIntPoint (iX (pt1 stOY)) (iY (pt1 stOX))) |
    stOX <- stsOX sts, stOY <- stsOY sts,
    stOX `shouldIntersect` stOY 
    ]
    where
        stsOX = filter ((== "oX") . streetAxis)
        stsOY = filter ((== "oY") . streetAxis)
        xAligned (StreetPP _ st1p1 st1p2) (StreetPP _ st2p1 st2p2) =
            (iX st1p1 <= iX st2p1) && (iX st2p1 <= iX st1p2)
        xAligned _ _ = error "StreetPP required"
        yAligned (StreetPP _ st1p1 st1p2) (StreetPP _ st2p1 st2p2) =
            (iY st2p1 <= iY st1p1) && (iY st1p1 <= iY st2p2)
        yAligned _ _ = error "StreetPP required"
        stX `shouldIntersect` stY = xAligned stX stY && yAligned stX stY
    

-- |should we make it return a pair instead of a list to 
-- -- |highlight the fact that there are 2 deadends?
deadendsOfStreet :: Shape -> [Busstop]
deadendsOfStreet st@(StreetPP _ pt1 pt2) = [
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
defaultBusstopDistance :: Int
defaultBusstopDistance = 5

-- | OLD UGLY VARIANT. DEPRECATED
--extraBusstopsForStreet' :: Shape -> [Busstop] 
--extraBusstopsForStreet' st = 
    --case st of (StreetPD _ pt dir len) -> extraBusstops $ minEnd p1 (streetPDP2 st)
               --(StreetPP _ pt1 pt2) -> extraBusstops $ minEnd pt1 pt2
               --_ -> streetRequiredError
    --where
        --minEnd p1 p2 = MkIntPoint (min (iX p1) (iX p2)) (min (iY p1) (iY p2))
        --d = defaultBusstopDistance
        --extraBusstops p1 = map (Extra st ) (extraBusstopsPoints p1) --PARTIAL APPLICATION WITH MAP WOWIE
        --extraBusstopsPoints p1 = if streetAxis st == "oX"  
                --then -- p1 must be the left end  -- iterating xs
                    --map (`MkIntPoint` iY p1) (takeWhile (<= (iX p1 + len - 2)) (iterate (+d) (iX p1)))
                --else -- if streetAxis st == "oY"   -- p1 must be the upper end   -- iterating ys
                    --map (MkIntPoint (iX p1)) (takeWhile (<= ((iY p1) + len - 2)) (iterate (+d) (iY p1)))
        
-- Other implementation using (mod D == 0) predicate
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