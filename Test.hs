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