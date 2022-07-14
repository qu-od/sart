{-# LANGUAGE ViewPatterns #-}

module Renderer
( Frame
, frame012
) where

import Pixels
    ( IntPoint (MkIntPoint, iX, iY)
    , GenColor (MkGenColor)
    , GenPixel (MkGenPixel)
    , xy
    , palette
    , lookupColor
    , colored
    , toPairs'
    )

import Shapes
    ( Shape (
        Building, bdName, ulp, lrp,
        Street, stName, pt1, pt2
        )
    , testShapes
    , streetAxis
    , interpolate'
    , isStreet
    , streetRequiredError
    )

import Busstops
    ( Busstop (
        Intersection,
        Deadend, 
        Extra
        )
    )

import Data.List ( 
    nub, sort, reverse, groupBy, intersperse, intercalate
    )
import Data.Char (
    isUpper, isDigit, isLower
    )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Function ( on )
import Data.Maybe (fromMaybe)


--------------------------------- TYPES ----------------------------------------
-------------- Frame
type Frame a = Map.Map IntPoint (GenColor a)
-- type synonyms can't be instantiated


------------------------------- CONSTANTS --------------------------------------
screenWidth :: Int
screenWidth = 160

screenHeight :: Int
screenHeight = 40

screenSize :: (Int, Int)
screenSize = (screenWidth, screenHeight)

screenXs :: [Int]
screenXs = [0 .. screenWidth - 1]

screenYs :: [Int]
screenYs = [0 .. screenHeight - 1]

backgroundPxs :: [GenPixel Char]
backgroundPxs = [
    MkGenPixel (MkIntPoint x y) (lookupColor "background") 
    | y <- screenYs, x <- screenXs
    ]


--------------------------------- WHEELS ---------------------------------------



---------------- 0.1.2 FRAME (Data Modules and verbose types) ------------------
------ frame012 ALGORITHM ------
-- |1. Get Shapes and Places (StreetPDs prohibited!)
-- |2. Form colored pixels for every Shape and Place
-- -- |Names could be rendered at that point (but won't)
-- -- |Also add background Pixels there
-- |3. Dump them in a Frame
-- -- |Duplicates are removed and Pixels ordered by coords because
-- -- -- |Frame Char = Map IntPoint (GenColor Char)
-- |4. Take only that pixels which are in-bouds of a frame coord matrix
-- |5. Form multiline string from a Frame

pixelsFromShape :: Shape -> [GenPixel Char]
pixelsFromShape bdg@Building {} = colored "building wall" (interpolate' bdg)
pixelsFromShape st@Street {} = colored "street" (interpolate' st)

-- |won't do a function for retrieving single pixel from a busstop because of
-- -- |generalization
-- |I mean may be we'll want a list of pixels
-- -- |from a single busstop in the future!
pixelsFromBusstop :: Busstop -> [GenPixel Char]
pixelsFromBusstop (Intersection _ _ p) =
    [MkGenPixel p $ lookupColor "intersection"]
pixelsFromBusstop (Deadend _ p) = [MkGenPixel p $ lookupColor "deadend"]
pixelsFromBusstop (Extra _ p) = [MkGenPixel p $ lookupColor "extra busstop"]

-------frame 012 pipeline
dumpPixels :: ([Shape], [Busstop], [GenPixel Char]) -> [GenPixel Char]
dumpPixels (shapes, busstops, miscPixels) = 
    -- Order in concat matters!
        -- first components have lower preсedence in rendering
    backgroundPxs 
    ++ concatMap pixelsFromShape shapes
    ++ concatMap pixelsFromBusstop busstops
    ++ miscPixels

-- |Map.fromList should also remove duplicates
frameMapFromPixels :: [GenPixel Char] -> Frame Char
frameMapFromPixels = Map.fromList . toPairs'

cropFrameBounds :: Frame Char -> Frame Char
cropFrameBounds = Map.fromList . filter inBounds . Map.toList
    where inBounds (MkIntPoint x y, _) = x `elem` screenXs && y `elem` screenYs

-- |Convert "Frame" Map to list of pairs
-- -- |Namely pairs of (IntPoint, GenColor Char)
-- |Group that list by iY of the Point. We'll get Matrix of pairs Then
-- |Flatten chars in every line of pair-matrix. We'll get list of strings
-- |Join strings with newline chars. We'll get a multiline string to show
showFrame :: Frame Char -> String
showFrame frame = ans
    where
        pixelPairs = Map.toList frame
        matrixOfPixelPairs = 
            groupBy (\(p1, _) (p2, _) -> iY p1 == iY p2) pixelPairs
        pixelPairsListToLine = 
            map (\(_, MkGenColor char) -> char)
        ans = unlines $ map pixelPairsListToLine matrixOfPixelPairs

---------frame012
frame012 :: ([Shape], [Busstop], [GenPixel Char]) -> String
frame012 = showFrame . cropFrameBounds . frameMapFromPixels . dumpPixels


--------------------------------------------------------------------------------
--FRAME_014 (with busstops re-generation, shape names rendering and fancy routes)
--------------------------------------------------------------------------------
frame014 :: ([Shape], [GenPixel Char]) -> String
frame014 = undefined