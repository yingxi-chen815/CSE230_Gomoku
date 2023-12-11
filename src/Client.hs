module Client (startClient, placePawn, getEnemyPawn, handleGame) where

import Network.Socket
import System.IO
import Control.Monad (unless, when)
import Logic (WholeState(..), WholeBoard(..), BoardDotStat(..), PlayerSide(..),getDotStat, placePawn, iPlacePawn, enemyPlacePawn, initWholeState,fetchTurn, PlayerSide, WinStat(..))
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

iPlacePawnAtCursor :: Handle -> WholeState -> IO (Either String WholeState)
iPlacePawnAtCursor handle (WholeState wb ps (y, x) b ws) = do
    let placePawnResult = placePawn (WholeState wb ps (y, x) b ws) (y, x) True
    case placePawnResult of
        Right newWholeState -> do
            -- 有效移动，更新服务器
            sendMyPawn (y, x) handle
            return $ Right newWholeState
        Left errorMsg -> 
            -- 无效移动，返回错误信息
            return $ Left errorMsg

-- updateEnemyPawn :: Handle -> IO WholeState
-- updateEnemyPawn handle = do
--     -- 向服务器发送请求
--     hPutStrLn handle "request latest wholestate"
--     -- 等待并接收响应
--     response <- hGetLine handle
--     -- 解析响应为 WholeState
--     let wholeState = parseWholeState response-- 解析逻辑 (依赖于数据格式)
--     -- 返回 WholeState
--     return wholeState

updateEnemyPawn :: Handle -> WholeState -> IO (Either String WholeState)
updateEnemyPawn handle ws = do
    (y,x)<-getEnemyPawn handle
    return (enemyPlacePawn ws (y,x)) 
-- initBoard::Handle->IO WholeBoardState

-- -- 解析从服务器接收到的字符串为 WholeState
-- parseWholeState :: String -> WholeState
-- parseWholeState str =
--     let [boardStr, playerStr, coordStr, boolStr, winStatStr] = splitOn ";" str
--         board = parseWholeBoard boardStr
--         playerSide = parsePlayerSide playerStr
--         (x, y) = parseCoordinate coordStr
--         boolValue = read boolStr :: Bool
--         winStat = parseWinStat winStatStr
--     in WholeState board playerSide (x, y) boolValue winStat


-- -- 解析 BoardDotStat
-- boardDotStatCons :: Char -> BoardDotStat
-- boardDotStatCons 'b' = BlackPawn
-- boardDotStatCons 'w' = WhitePawn
-- boardDotStatCons '.' = EmptyDot

-- -- 将单行字符串转换为 BoardDotStat 列表
-- fromString :: String -> [BoardDotStat]
-- fromString str = map boardDotStatCons str

-- -- 将字符串列表转换为二维 BoardDotStat 列表
-- fromStrings :: [String] -> [[BoardDotStat]]
-- fromStrings strs = map fromString strs

-- -- 解析整个棋盘
-- parseWholeBoard :: String -> WholeBoard
-- parseWholeBoard str =
--     let sizeAndRows = splitOn ";" str
--         size = read (head sizeAndRows) :: Int
--         rows = tail sizeAndRows
--         boardRows = fromStrings rows
--     in WholeBoard size boardRows

-- -- 解析 PlayerSide
-- parsePlayerSide :: String -> PlayerSide
-- parsePlayerSide "BlackPlayer" = BlackPlayer
-- parsePlayerSide "WhitePlayer" = WhitePlayer
-- -- 添加其他可能的玩家方

-- -- 解析坐标
-- -- parseCoordinate :: String -> (Int, Int)
-- -- parseCoordinate s = let [x, y] = map read $ splitOn "," s in (x, y)

-- -- 解析 WinStat
-- parseWinStat :: String -> WinStat
-- parseWinStat "InGame" = InGame
-- parseWinStat "Win" = IWin
-- parseWinStat "Lose" = EnemyWin

initBoard::Handle->IO WholeState
initBoard handle = do
    sideMsg <- hGetLine handle
    let isPlayerBlack = sideMsg == "You are Black"
    let size = 10
    let playCh = if isPlayerBlack then 'b' else 'w'
    let wholeState = initWholeState size playCh
    return wholeState
