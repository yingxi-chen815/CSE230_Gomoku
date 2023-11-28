module Main (main) where

import System.IO
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

import Logic(BoardDotStat,boardDotStatCons,
             WholeBoard,initWholeBoard,getDotStat,initWholeState,
             moveCursor,iPlacePawn,placePawnAtCursor,enemyPlacePawn,fetchWholeState)

import Server(startServer)
import Client(startClient)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["server"] -> startServer
        -- ["client", host, port] -> startClient host port
        ["client"] -> startClient "127.0.0.1" "3000"
        _ -> putStrLn "Invalid arguments"
