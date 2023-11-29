module Client (startClient, placePawn, getEnemyPawn, handleGame) where

import Network.Socket
import System.IO
import Control.Monad (unless, when)
import Logic (WholeState, placePawn, iPlacePawn, enemyPlacePawn, initWholeState,fetchTurn)
import Data.List.Split (splitOn)

type Coordinate = (Int, Int)

-- 连接到服务器
startClient :: String -> String -> IO Handle
startClient host port = withSocketsDo $ do
    addrinfos <- getAddrInfo Nothing (Just host) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering
    return handle

-- 发送坐标到服务器
sendMyPawn :: Coordinate -> Handle -> IO ()
sendMyPawn (x, y) handle = do
    hPutStrLn handle (show x ++ "," ++ show y)

-- 从服务器获取敌方坐标
getEnemyPawn :: Handle -> IO Coordinate
getEnemyPawn handle = do
    coordStr <- hGetLine handle
    return $ parseCoordinate coordStr

parseCoordinate :: String -> Coordinate
parseCoordinate s = let [x, y] = map read $ splitOn "," s
                    in (x, y)

-- 处理游戏逻辑
-- 在 handleGame 函数中处理角色分配
handleGame :: Handle -> IO ()
handleGame handle = do
    sideMsg <- hGetLine handle
    putStrLn sideMsg -- 打印出角色信息

    let isPlayerBlack = sideMsg == "You are Black"
    let size = 10
    let playCh = if isPlayerBlack then 'b' else 'w'
    let whState = initWholeState size playCh

    gameLoop handle whState isPlayerBlack

-- 游戏循环
gameLoop :: Handle -> WholeState -> Bool -> IO ()
gameLoop handle whState isPlayer = do
    print whState
    if isPlayer
    then do
        coord <- getCoordinatesFromCommandLine
        case updateBoardState whState coord isPlayer of
            Left err -> do
                putStrLn err
                gameLoop handle whState isPlayer
            Right newState -> do
                sendMyPawn coord handle
                gameLoop handle newState (not isPlayer)
    else do
        coord <- getEnemyPawn handle
        case updateBoardState whState coord isPlayer of
            Left err -> do
                putStrLn err
                gameLoop handle whState isPlayer
            Right newState -> do
                gameLoop handle newState (not isPlayer)

-- 更新棋盘状态
updateBoardState :: WholeState -> Coordinate -> Bool -> Either String WholeState
updateBoardState state coord isPlayer = if isPlayer
                                        then iPlacePawn state coord
                                        else enemyPlacePawn state coord


-- 从命令行获取并解析坐标
getCoordinatesFromCommandLine :: IO Coordinate
getCoordinatesFromCommandLine = do
    putStrLn "Enter your move (format: x,y):"
    hFlush stdout  -- 确保提示信息立即打印
    parseCoordinate <$> getLine