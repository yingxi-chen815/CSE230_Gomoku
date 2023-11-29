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
        -- ["client"] -> startClient "https://super-duper-orbit-4vpvv7xw7pgcq4p6-3000.app.github.dev/" "3000"
        _ -> putStrLn "Invalid arguments"
