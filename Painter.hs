module Painter
(frame
) where

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

--------------------------GEOMETRICAL SHAPES -----------------------------------
-- WHIT PAPIR))0)0
-- use guards to fit shapes in a frame
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




----------------ACTUAL ACTUAL FRAME PAINTER (MULTISYMBOLIC) --------------------
scnd :: (a, b, c) -> b
scnd (_, y, _) = y

thrd :: (a, b, c) -> c
thrd (_, _, z) = z

-- Use foldl instead
concatStrings :: String -> [String] -> String
concatStrings accumulator [] = accumulator --breaks the loop
--concatStrings "" strings = concatStrings ("" ++ head strings) (tail strings) --starts folding CAN BE REMOVED
concatStrings accumulator strings = 
    concatStrings (accumulator ++ head strings) (tail strings) --ordinary iteration

frameLine :: [(Int, Int, String)] -> Int -> String
frameLine screen lineNum = 
    concat [thrd pixel | pixel <- screen, scnd pixel == lineNum]

frame :: [(Int, Int, String)] -> (Int, Int) -> [String]
frame screen (width, height) = 
    [frameLine screen lineNum | lineNum <- [0..height-1]]
