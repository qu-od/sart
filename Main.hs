import Painter
    ( IntPoint (MkIntPoint)
    , GenColor (MkGenColor)
    , GenPixel (MkGenPixel)
    , Frame
    , Direction (NoDirection, Up, Down, Left', Right')
    , Shape (EmptyShape, Building, StreetPD, StreetPP, Route)
    , Place (Intersection, Deadend, Busstop) -- for advanced routes
    , frame012
    , paint
    )

import Shapes 
    ( Figure (MakeFigure) -- deprecated  
    , line -- deprecated  
    , box -- deprecated  
    , building -- deprecated  
    , street -- deprecated  
    )


-------------------------- QUESTIONS -------------------------------------------
-- how do you choose between let, where, guards, cases, ifs and matternmatching?
-- $ и . с чем едят
-- Куда класть определения типов с конструкторами. В отдельный модуль?
--

------------------------------- TODO -------------------------------------------
-- Ultimate goal: automate routes making.
    -- Apply 2 points in the city and let the program to decide how to lay it out!
-- REFACTOR PAINTER IN A RECURSIVE MANNER. REDUCE RUNNING TIME
-- figure out the rules of indentation and multiline expressions
-- how to make exceptions for special cases? (pattern matching and guards will help)
-- optimize frame rendering with kewl alghorithms
-- handle more exceptions


----------------------------------- TYPES --------------------------------------


--------------------------- FUNCS FOR MAIN -------------------------------------


----------------------------- MAIN ---------------------------------------------
testPoint :: IntPoint
testPoint = MkIntPoint 2 3

testCharGenColor :: GenColor Char
testCharGenColor = MkGenColor '$'

testGenPixel :: GenPixel Char
testGenPixel = MkGenPixel testPoint testCharGenColor

anotherTestGenPixel :: GenPixel a
anotherTestGenPixel = MkGenPixel (MkIntPoint 3 4) MkGenColor '-'  

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
    Building "TEST BUILDING 1"  (p 20 0)  (p 42 4),
    Building "5букв"            (p 50 1)  (p 56 4),
    Building "TEST BUILdinG 68" (p 40 4)  (p 60 6),
    Building " в ебенях"        (p 44 14) (p 70 19),
    Building "что-то"           (p 74 4)  (p 80 6),
    -- 
    StreetPD "Vo"               (p 150 5) Left'  50,
    StreetPD "TUPIKOVYI TYPIK"  (p 150 3) Right' 20,
    StreetPD "Бельфегоровская"  (p 50 5)  Up     5,
    --
    StreetPP "Gayorgyeva"       (p 150 3) (p 120 3),
    StreetPP "Svobody"          (p 100 5) (p 150 5),
    StreetPP "Vo"               (p 120 0) (p 120 5),
    StreetPP "TUPIKOVYI TYPIK"  (p 150 2) (p 399 2),
    StreetPP "Бельфегоровская"  (p 171 2) (p 228 2),
    --
    Route 1 (zipWith p [40, 44, 44]        [4, 4, 14]),
    Route 2 (zipWith p [136, 149, 149, 10] [2, 2, 6, 6]),
    Route 3 (zipWith p [10, 10, 134, 134]  [6, 4, 4, 1])
    ]

takeStreets :: [Shape] -> [Shape]
takeStreets = undefined

intersectionsOfStreets :: [Shape] -> [Place]
intersectionsOfStreets = undefined

deadendsOfStreets :: [Shape] -> [Place]
deadendsOfStreets = undefined

busstopsForStreet :: Shape -> [Place]
busstopsForStreet = undefined

testPlaces :: [Place]
testPlaces = concat [
    intersectionsOfStreets streets,
    deadendsOfStreets streets, 
    concatMap busstopsForStreet streets -- OH MY
    ]
    where 
        streets = takeStreets testShapes


--------------------------------------- MAIN -----------------------------------
renderFrame :: IO ()
renderFrame = putStr $ frame012 testShapes testPlaces
