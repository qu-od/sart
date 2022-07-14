{-# LANGUAGE ViewPatterns #-}

import Pixels
    ( IntPoint (MkIntPoint, iX, iY)
    , GenColor (MkGenColor)
    , GenPixel (MkGenPixel)
    , palette
    , lookupColor
    )

import Renderer
    ( Frame
    , frame012
    )

import Shapes
    ( Shape (
        Building,
        Street, stName
        )
    , streetLen
    , streetAxis
    , interpolate'
    , isStreet
    , streetRequiredError
    )

import GameState
    ( Entry (entry)
    , GameStateString
    , FrameContents
    , GameMeta
    , InputLine
    , entryPrefix
    , getFrameContents
    , updateGameState
    , testGameState
    )

import Control.Monad 
    ( when
    , sequence
    , forM
    , mapM
    , forever
    )

import qualified Data.Map as Map

import System.IO (
    openFile, hGetContents, hClose, IOMode(ReadMode), hPutStr
    , openTempFile
    )
import System.Directory (removeFile, renameFile)


------------------------------ META --------------------------------------------
-- use functors, applicatives and monads (Maybe, List and IOs)
-- DON'T YOU DARE TO OVEROPTIMIZE! IT WORKS? IF YES - MOVE ON.
-- SART WILL BE DONE IN THIS WEEK NO MATTER HOW FAR WE'LL GO 

-------------------------- QUESTIONS -------------------------------------------
-- may I carry state as an argument in the reqursive main?
-- -- or it'd be better to save state in a file and load it everytime?
-- -- -- I think I should try to carry state without file-jerk on every frame somehow


----------------------------- TODO ---------------------------------------------
-- debug (5 edits are enough)
-- shapes to Shapes stops and pixels stays in Painter.
        --Funcs about game state from Main in Saves
        --Entry to Saves!
-- build busstops inside a frame012! (but make their entries in save anyway)
    -- (busstops are fully defined by streets after all)
-- state of frame and a cursor saved in a file (0.1.3)
-- user cursor 
    -- move it with WASD input
    -- action button (to build shapes in-console)
    -- feedback for actions (0.1.4)
-- render names of the 
    -- streets (0.1.5)
    -- buildings 
    -- routes (0.1.6)
-- automatic city generation (0.2)
-- advanced routes
    -- bind them to street grid (0.2.1)
    -- render in parallel to the streets and to each other (0.2.2)
    -- autogenerate route from point A to point B (0.2.3)
-- animate (0.3....)
    -- busses traffic
    -- other traffic
    -- responsive traffic (traffic jams must occur)
--ENOUGH.

----------------------------- CONFIG ----------------------------------------
gameStateFileName :: String
gameStateFileName = "game state.txt"



------------------------------ test functions ----------------------------------



-------------------------------- IO SAVES --------------------------------------
makeFileWithInitialGameState :: IO ()
makeFileWithInitialGameState = do 
    writeFile gameStateFileName testGameState

loadGameFromAFile :: IO GameStateString
loadGameFromAFile = readFile gameStateFileName

saveGameIntoANewFileAndDeleteOldFile :: GameStateString -> IO ()
saveGameIntoANewFileAndDeleteOldFile gameStateString = do 
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle gameStateString
    hClose tempHandle
    removeFile gameStateFileName
    renameFile tempName gameStateFileName


---------------------------------- IO GAME -------------------------------------
renderFrame :: FrameContents -> IO ()
renderFrame fContents = putStrLn $ frame012 fContents

funcc :: Maybe GameStateString -> GameStateString -> IO GameStateString
funcc (Just newGameStateString) _ = return newGameStateString
funcc _ prevGameStateString = do
    putStrLn "wrong Key, mate"
    return prevGameStateString

-- |if user input is not " ", calls itself to do next stage of the game
-- |and when user input is " ", returns the last GameState
recursiveLoopOfGameStagesWithoutJerkingAFile :: GameStateString -> IO GameStateString
recursiveLoopOfGameStagesWithoutJerkingAFile prevGameStateString = do
    () <- renderFrame $ getFrameContents prevGameStateString
    userActionLine <- getLine
    if userActionLine /= " "
        then do
            let maybeNewGameStateString = updateGameState prevGameStateString userActionLine
            updatedGameStateString <- funcc maybeNewGameStateString prevGameStateString
            saveGameIntoANewFileAndDeleteOldFile updatedGameStateString --FOR FOR LAST STAGE STATE LOGGING IN CASE OF A FAILURE
            recursiveLoopOfGameStagesWithoutJerkingAFile updatedGameStateString
        else return prevGameStateString

infiniteStagesGame :: IO ()
infiniteStagesGame = do
    makeFileWithInitialGameState --SAVING TEST GAME STATE AS INITIAL!
    initialGameState <- loadGameFromAFile
    lastGameState <- recursiveLoopOfGameStagesWithoutJerkingAFile initialGameState
    -- when " " unput occured, reqursion ended and we're making an autosave
    saveGameIntoANewFileAndDeleteOldFile lastGameState


--------------------------------- MAIN -----------------------------------------
main :: IO ()
main = infiniteStagesGame

