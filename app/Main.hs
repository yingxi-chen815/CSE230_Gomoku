module Main where

import System.Environment (getArgs)
import Client (startClient,handleGame)
import Server (startServer)  -- 假设这是您的服务器启动函数
import Logic(BoardDotStat,boardDotStatCons,
             WholeBoard,initWholeBoard,getDotStat,initWholeState,
             moveCursor,iPlacePawn,placePawnAtCursor,enemyPlacePawn,fetchWholeState)

main :: IO ()
main = do
    args <- getArgs
    case args of
        -- 启动客户端
        ["client"] -> do
            putStrLn "Starting client..."
            handle <- startClient "127.0.0.1" "3000"
            handleGame handle
        -- 启动服务器
        ["server"] -> do
            putStrLn "Starting server..."
            startServer
        _ -> putStrLn "Usage: program (server | client <host> <port>)"
