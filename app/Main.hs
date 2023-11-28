module Main (main) where

import System.IO
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

import Logic(BoardDotStat,boardDotStatCons,
             WholeBoard,initWholeBoard,getDotStat,initWholeState,
             moveCursor,iPlacePawn,placePawnAtCursor,enemyPlacePawn,fetchWholeState)

import Server(startServer)
import Client(startClient)

-- main::IO ()
-- main = do
--     hSetEncoding stdout utf8
--     putStrLn "Enter the size of the board:"
--     size <- readLn :: IO Int
--     -- let whState = initWholeState size color
--     let whState = initWholeState size 'b'

--     let st1 = fetchWholeState (iPlacePawn whState (5,5))
--     let st2 = fetchWholeState (enemyPlacePawn st1 (6,5))
--     let st3 = fetchWholeState (iPlacePawn st2 (5,6))
--     let st4 = fetchWholeState (enemyPlacePawn st3 (6,6))
--     let st5 = fetchWholeState (iPlacePawn st4 (5,7))
--     let st6 = fetchWholeState (enemyPlacePawn st5 (6,7))
--     let st7 = fetchWholeState (iPlacePawn st6 (5,8))
--     let st8 = fetchWholeState (enemyPlacePawn st7 (6,8))
--     let st9 = fetchWholeState (iPlacePawn st8 (5,9))
    
--     putStrLn (show st9)


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["server"] -> startServer
        -- ["client", host, port] -> startClient host port
        ["client"] -> startClient "127.0.0.1" "3000"
        _ -> putStrLn "Invalid arguments"
