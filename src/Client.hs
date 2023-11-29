module Client(startClient) where

import Network.Socket
import System.IO
import Control.Monad (when, unless)
import Data.List.Split (splitOn)

import Logic(BoardDotStat(..),boardDotStatCons,
             WholeBoard(..),initWholeBoard,getDotStat,WholeState(..),initWholeState,getCorrespondentPawn,
             moveCursor,iPlacePawn,placePawnAtCursor,enemyPlacePawn,placePawn,fetchWholeState, PlayerSide, WinStat)
import Data.List (isPrefixOf)

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
    handleServerMessages handle
    where
        handleServerMessages handle = do
            -- 读取并打印服务器响应
            serverResponse <- hGetLine handle
            putStrLn $ "Server says: " ++ serverResponse

            -- 根据服务器请求的不同响应相应
            case serverResponse of
                "Choose your side (Black/White):" -> do
                    -- 让用户选择一边
                    side <- getLine
                    hPutStrLn handle side

                "Enter the size of the board:" -> do
                    -- 让用户输入棋盘大小
                    size <- getLine
                    hPutStrLn handle size
                
                "Send your move (format: x,y):" -> do
                    -- 让用户输入坐标
                    coord <- getLine
                    hPutStrLn handle coord
                
                "Received" -> do
                    -- 打印服务器确认接收坐标的消息
                    putStrLn "Server has received the move."
                
                "quit" -> return ()

                -- 处理棋盘状态信息
                response | "Serialized board state:" `isPrefixOf` response -> do
                    putStrLn $ "Server says: " ++ response
                    let boardState = drop (length "Serialized board state: ") response
                    putStrLn $ "Board state: " ++ boardState
                
                -- 处理错误信息
                response | "Error:" `isPrefixOf` response -> do
                    putStrLn $ "Server says: " ++ response
                
                -- 退出
                -- TODO

                _ -> return ()  -- 其他类型的消息不需要特殊处理

            -- -- 当用户输入不是'quit'时，继续
            -- unless (line == "quit") $ do
            --     -- 继续循环
            handleServerMessages handle
        
        waitForReceive handle = do
            receiveMsg <- hGetLine handle
            putStrLn $ "Server says: " ++ receiveMsg
            unless (receiveMsg == "quit") $ do
                handleServerMessages handle


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