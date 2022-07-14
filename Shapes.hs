{-# LANGUAGE ViewPatterns #-}

module Shapes 
( Shape (
    Building, bdName, ulp, lrp,
    Street, stName, pt1, pt2
    )
, testShapes
, streetLen
, streetAxis
, interpolate'
, isStreet
, streetRequiredError
) where

import Pixels
    ( IntPoint (MkIntPoint, iX, iY)
    , GenColor (MkGenColor)
    , GenPixel (MkGenPixel)
    , xy
    , leftX
    , rightX
    , upperY
    , lowerY
    ) 

---------------------------- SHAPE TYPES ----------------------------------------
-------------- Direction
data Direction = --unused for now... (0.1.3)
      Up
    | Down
    | Left'
    | Right'
    deriving (Eq)

instance Show Direction where
    show Up = "Upwards"
    show Down = "Downwards"
    show Left' = "to the Left"
    show Right' = "to the Right"

----------------- Polyline
-- type Polyline = [IntPoint]

---------------- Shape
data Shape = -- GENERALIZE ALL THOSE WITH A BOX AND A POLYLINE
        -- record syntax DISCOURAGED when multiple value constructors are used
        -- ADDING DIFFERENT CONSTRUCTORS FOR A STREET WAS A MISTAKE))
    Building {
        bdName :: String,
        ulp :: IntPoint, -- upper left point
        lrp :: IntPoint -- lower right point
        }
    | Street {
        stName :: String,
        pt1 :: IntPoint,
        pt2 :: IntPoint
        }

instance Show Shape where 
    show (Building name pt1 pt2) = concat [
        "Building \"", name, "\" between ", xy pt1, "; ", xy pt2
        ]
    show (Street name pt1 pt2) = concat [
        name, "Street from ", xy pt1, "to ", xy pt2
        ]

streetLen :: Shape -> Int -- bigPP
streetLen (Street _ (MkIntPoint x1 y1) (MkIntPoint x2 y2))
    | y1 == y2 = 1 + abs (x2 - x1) -- horizontal
    | x1 == x2 = 1 + abs (y2 - y1) -- vertical
    | otherwise = error "Diagonal street occured which is forbidden"
streetLen _ =
    error "streetLen function requires Street and not any other Shape"

-- |single-point street case is not handled specifically
streetAxis :: Shape -> String
streetAxis (Street _ (MkIntPoint x1 y1) (MkIntPoint x2 y2) )
    | x1 == x2 = "oY"
    | y1 == y2 = "oX"
    | otherwise = error "Unexpected street Direction"
streetAxis _ = streetRequiredError

-- |that's basically a type "method"
-- |function doesn't paint buildings' walls into special color
-- -- |and doesn't render name of a shape
interpolate' :: Shape -> [IntPoint]
interpolate' (Building _ pt1 pt2) = [
    MkIntPoint x y 
    | x <- [leftX pts .. rightX pts], y <- [upperY pts .. lowerY pts]
    ]
    where pts = [pt1, pt2]
interpolate' st@(Street _ pt1 pt2)
    | streetAxis st == "oX" = 
        map (`MkIntPoint` iY pt1) [leftX pts .. rightX pts]
    | otherwise = map (MkIntPoint (iX pt1)) [upperY pts .. lowerY pts]
    where pts = [pt1, pt2]

isStreet :: Shape -> Bool
isStreet Street {} = True
isStreet _ = False

-- which type need to be used?
streetRequiredError :: a
streetRequiredError = error "There was a Shape... But there was no STREET!"


----------------------------- TEST DATA ----------------------------------------
p :: Int -> Int -> IntPoint
p = MkIntPoint

testShapes :: [Shape]
testShapes = [ -- leading elements have higher priority in rendering
    Building "TEST BUILDING 1"  (p  20  0) (p  42  4),
    Building "5букв"            (p  50  1) (p  56  4),
    Building "TEST BUILdinG 68" (p  40  4) (p  60  6),
    Building " в ебенях"        (p  44 14) (p  70 19),
    Building "что-то"           (p  74  4) (p  80  6),
    --
    Street "foo"                (p 150  5) (p 100  5),
    Street "Yablochkova"        (p 150  3) (p 170  3),
    Street "Kosmonavtov"        (p  50  5) (p  50  0),
    Street "Gayorgyeva"         (p 150  3) (p 120  3),
    Street "Svobody"            (p 100  5) (p 150  5),
    Street "Vo"                 (p 120  0) (p 120  5),
    Street "TUPIKOVYI TYPIK"    (p 150  2) (p 399  2),
    Street "Бельфегоровская"    (p 171  2) (p 228  2),
    Street "test"               (p   6  8) (p 103  8),
    Street "test 2"             (p  72  0) (p  72 13)
    ]
