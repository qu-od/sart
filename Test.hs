import Data.Function ( on )


--------------------- BASICS TESTING ----------------------------
a = [1, 2, 3]
b = [4, 5, 6]
showTest = show a
-- main identifier is restricted
myMain = showTest

--------------------- SCREEN TEST ----------------------------

line = take 100 (cycle "--")
emptyPicture = replicate 10 line
cornersCoords = [[0, 0], [0, 9], [99, 0], [99, 9]]
filterCoordsForLineWUEL lineNum coords = [ --WUEL means With Unsifted Empty Lists
    if head coord == lineNum then coord else [] | coord <- coords 
    --empty list means don't paint
    ]
removeEmptyListsFromList rawList = [x | x <- rawList, not (null x)]
-- [[Int]] -> [Int]
projectCoordsOn1stAxis = map head --PAUSED
--coordsForLine = removeEmptyListsFromList filterCoordsForLineWUEL -- would be nice to use it
--coordsArrangedInLines coords = [ --[[Int]] -> [[[Int]]]
  --  removeEmptyListsFromList \
    --[filterCoordsForLine lineNum coords | lineNum <- [0 .. pictureHeight-1]]
    --]
-- coordsToPaint = coordsArrangedInLines
--paintedPicture emptyPicture coordsToPaint = -- [String] -> [[Int]] -> [String]
  --  []

---------------------ACTUAL PAINTER (MULTISYMBOLIC)--------------------------

--assuming that pixels in screen are ordered already

--addNewCharToAString stringHead nextChar = stringHead ++ nextChar
myyFst (x, _, _) = x -- lol what?
myyThrd (_, _, x) = x -- lol what?
addNewLineToAPicture pictureHead nextLine = pictureHead ++ "\n" ++ nextLine
--screenLine lineNum screen = foldl (++) [myyThrd pixel | pixel <- screen, myyFst pixel == lineNum]
--screenLines screen screenSize = [
  --  foldl addNewLineToAPicture [screenLine lineNum screen | lineNum <-  [0 .. (snd screenSize) - 1]]
    --]


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
------------------ FUNCTION SYNTAX TESTING -------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
tellWhatNumber :: (Integral a) => a -> String --typeclasses ad-hoc
tellWhatNumber 2 = "Two"
tellWhatNumber 5 = "Five"
tellWhatNumber num = "That's not 2 or 5. idk"

fu :: (Int, Int) -> (Int, Int) -> (Int, Int)
fu (x1, y1) (x2, y2)
    | (x_sum < 0) && (y_sum < 0) = ((-1) * x_sum, (-1) * y_sum)
    | (x_sum < 0) && (y_sum >= 0) = ((-1) * x_sum, y_sum)
    | (x_sum >= 0) && (y_sum < 0) = (x_sum, (-1) * y_sum)
    | otherwise = (x_sum, y_sum)
    where x_sum = x1 + x2
          y_sum = y1 + y2

-------------------------- LISTS PATTERN MATCHING ------------------------------
testNums :: [Int]
testNums = [1, 2, 3, 4, 5, 6]

threeNums :: [Int]
threeNums = [1, 2, 3]

twoNums :: [Int]
twoNums = [1, 2]

oneNum :: [Int]
oneNum = [1]

--getFirstThreeElemsFromListAndTheRest :: (Num a) => [a] -> (a, a, a, [a])
getFirstThreeElemsFromListAndTheRest :: [a] -> (a, a, a, [a])
getFirstThreeElemsFromListAndTheRest [_, _] = 
    error "len of list is 2. minimum 3."
getFirstThreeElemsFromListAndTheRest [_] = error "len of list is 1. minimum 3."
getFirstThreeElemsFromListAndTheRest [] = error "len of list is 0. minimum 3."
getFirstThreeElemsFromListAndTheRest (fst:snd:thrd:theRest) =
    (fst, snd, thrd, theRest)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' item (x:xs)
    | item == x = True
    | otherwise = elem' item xs


------------------------ PARTIAL APPLICATION -----------------------------------

multThreeNumsTogether :: (Num a) => a -> (a -> (a -> (a)))
multThreeNumsTogether x y z = x * y * z

addThirty' :: (Num a) => a -> a
addThirty' x = (+) 30 x

addThirty :: (Num a) => a -> a
addThirty = (+) 30

subctFrom10 :: (Num a) => a -> a
subctFrom10 = (-) 10

subct10 :: (Num a) => a -> a
subct10 = (flip (-)) 10 -- WTF THOSE PARENS ARE REDUNDANT??! yep. cuz part. app.

listOfMultiplications :: (Enum a, Num a) => [a -> a]
listOfMultiplications = map (*) [0..]

zipWith' :: (Enum a, Enum b, Enum c) => (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

doTwice :: (a -> a) -> (a -> a)
doTwice f = f . f  --NEED TESTING!

quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' [x] = [x]
quickSort' (pivot:xs) = sortedLessers ++ [pivot] ++ sortedGreaters
    where
        sortedLessers  = quickSort' [x | x <- xs, x <= pivot]
        sortedGreaters = quickSort' [x | x <- xs, x > pivot]

quickSort'' :: (Ord a) => [a] -> [a]
quickSort'' [] = []
quickSort'' [x] = [x]
quickSort'' (pivot:xs) = sortedLessers ++ [pivot] ++ sortedGreaters
    where
        sortedLessers  = quickSort' $ filter (<= pivot) xs
        sortedGreaters = quickSort' $ filter (>  pivot) xs


--------------------------------------------------------------------------------
------------------------------ TYPECLASSES TEST --------------------------------
--------------------------------------------------------------------------------
data FloatPoint = MakeFloatPoint {xx :: Float, yy :: Float} deriving (Eq)
--superclasses will be instanctated manually

data GenericPoint a = MakeGenericPoint {gx :: a, gy :: a} deriving (Show)
--instantiate type of a kind * -> * manually later 

class PointEqFoo concrType where --gimmick typeclass. OH MY!
    (==%^#==) :: concrType -> concrType -> Bool
    (/==%^#==) :: concrType -> concrType -> Bool
    x ==%^#== y = not (x /==%^#== y) -- how TF mutual recursion works?
    x /==%^#== y = not (x ==%^#== y)

instance PointEqFoo FloatPoint where -- OH MY
    flPt1 ==%^#== flPt2  =
        ((==) `on` xx) flPt1 flPt2 && ((==) `on` yy) flPt1 flPt2
    flPt1 /==%^#== flPt2 =
        ((/=) `on` xx) flPt1 flPt2 || ((/=) `on` yy) flPt1 flPt2

instance Show FloatPoint where -- OH MY (x2)
    show fPt = "custom Show instantiation with a FloatPoint.\n\
                \x = " ++ show (xx fPt) ++ "y = " ++ show (yy fPt)

instance Ord FloatPoint where -- Ord is a subclass of Eq!
    -- say we want to ord those points by Radius-vector
    compare = compare `on` r
        where 
            r fPt = sqrt $ xx fPt ^ 2 + yy fPt ^ 2

