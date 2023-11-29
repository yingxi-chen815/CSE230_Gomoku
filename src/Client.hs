module Client(startClient) where

import Network.Socket
import System.IO
import Control.Monad (when, unless)
import Data.List.Split (splitOn)

import Logic(BoardDotStat(..),boardDotStatCons,
             WholeBoard(..),initWholeBoard,getDotStat,WholeState(..),initWholeState,getCorrespondentPawn,
             moveCursor,iPlacePawn,placePawnAtCursor,enemyPlacePawn,placePawn,fetchWholeState, PlayerSide, WinStat)

type Coordinate = (Int, Int)

startClient :: String -> String -> IO ()
startClient host port = withSocketsDo $ do
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering
    interactWithServer handle
    hClose handle

interactWithServer :: Handle -> IO ()
interactWithServer handle = do
    putStrLn "Connected to server. Type 'quit' to exit."
    -- 读取并打印服务器响应
    serverResponse <- hGetLine handle
    putStrLn $ "Server says: " ++ serverResponse
    
    loop
    where
        loop = do
            -- 用户输入
            line <- getLine
            hPutStrLn handle line

            -- 读取并打印服务器响应
            serverResponse <- hGetLine handle
            putStrLn $ "Server says: " ++ serverResponse


            -- 当用户输入不是'quit'时，继续
            unless (line == "quit") $ do
                -- 继续循环
                loop



parseCoordinate :: String -> Coordinate
parseCoordinate s = let [x, y] = map read $ splitOn "," s
                    in (x, y)


-- deserializeWholeState :: String -> WholeState
-- deserializeWholeState s = let [boardStr, playerStr, cursorStr, flagStr, winStatStr] = splitOn ";" s
--                               board = deserializeWholeBoard boardStr
--                               player = read playerStr :: PlayerSide
--                               cursor = parseCoordinate cursorStr
--                               flag = read flagStr :: Bool
--                               winStat = read winStatStr :: WinStat
--                           in WholeState board player cursor flag winStat



-- deserializeWholeBoard :: String -> WholeBoard
-- deserializeWholeBoard s = let [sizeStr, boardStr] = splitOn ";" s
--                               size = read sizeStr :: Int
--                               board = map deserializeBoardDotStat $ splitOn "," boardStr
--                         in WholeBoard size [board]



-- deserializeBoardDotStat :: String -> BoardDotStat
-- -- deserializeBoardDotStat "0" = Left "Invalid board dot stat"
-- deserializeBoardDotStat "1" = BlackPawn
-- deserializeBoardDotStat "2" = WhitePawn
-- deserializeBoardDotStat "3" = EmptyDot