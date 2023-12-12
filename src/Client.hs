module Client (startClient, placePawn, getEnemyPawn, handleGame, initBoard, iPlacePawnAtCursor, updateEnemyPawn) where

import Network.Socket
import System.IO
import Control.Monad (unless, when)
import Logic
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
    -- sideMsg <- hGetLine handle
    -- putStrLn sideMsg -- 打印出角色信息

    -- let isPlayerBlack = sideMsg == "You are Black"
    -- let size = 10
    -- let playCh = if isPlayerBlack then 'b' else 'w'
    -- let whState = initWholeState size playCh

    whState@(WholeState wb ps (y,x) isMyTurn ws)<-initBoard handle
    print whState
    if ps==WhitePlayer then do
        enemyPwRes<-updateEnemyPawn handle whState
        case enemyPwRes of
                        Left err2->do
                            putStrLn err2
                            putStrLn "enemy cheating, game ends"
                        Right newState2->do
                            gameLoop handle newState2
    else do
        gameLoop handle whState

-- 游戏循环
gameLoop :: Handle -> WholeState -> IO ()
gameLoop handle whState@(WholeState wb ps (y,x) isMyTurn ws) = do
    print whState
    ch<-getCommand
    putStrLn ("your command: "++[ch])
    if ch=='w' then do
        let newWhState = moveCursor whState DirUp 
        gameLoop handle newWhState
    else if ch=='a' then do
        let newWhState = moveCursor whState DirLeft 
        gameLoop handle newWhState
    else if ch=='s' then do
        let newWhState = moveCursor whState DirDown 
        gameLoop handle newWhState
    else if ch=='d' then do
        let newWhState = moveCursor whState DirRight 
        gameLoop handle newWhState
    else if ch=='e' then do
        -- if ps==BlackPlayer then do
            placePwRes<-iPlacePawnAtCursor handle whState
            case placePwRes of 
                Left err->do
                    putStrLn err
                    gameLoop handle whState
                Right newState->do
                    print newState
                    putStrLn "\n"
                    enemyPwRes<-updateEnemyPawn handle newState
                    case enemyPwRes of
                        Left err2->do
                            putStrLn err2
                            putStrLn "enemy cheating, game ends"
                        Right newState2->do
                            gameLoop handle newState2
    else do
        gameLoop handle whState

    -- gameLoop handle whState

    -- print whState
    -- let isPlayer = isMyTurn
    -- if isPlayer
    -- then do
    --     coord <- getCoordinatesFromCommandLine
    --     case updateBoardState whState coord isPlayer of
    --         Left err -> do
    --             putStrLn err
    --             gameLoop handle whState
    --         Right newState -> do
    --             sendMyPawn coord handle
    --             gameLoop handle newState
    -- else do
    --     coord <- getEnemyPawn handle
    --     case updateBoardState whState coord isPlayer of
    --         Left err -> do
    --             putStrLn err
    --             gameLoop handle whState
    --         Right newState -> do
    --             gameLoop handle newState

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

mustGetLine::IO String
mustGetLine = do
    res<-getLine
    if res==""
        then
            mustGetLine
        else do
            return res

getCommand::IO Char
getCommand = do
    putStrLn "Enter your command char:"
    hFlush stdout
    line<-mustGetLine
    return (line!!0)


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

iPlacePawnUntilCorrect::Handle -> WholeState -> IO (Either String WholeState)
iPlacePawnUntilCorrect handle (WholeState wb ps (y, x) b ws) = do
    iPlacePawnAtCursorRes <- iPlacePawnAtCursor handle (WholeState wb ps (y, x) b ws)
    case iPlacePawnAtCursorRes of
        Right newWholeState->do
            return iPlacePawnAtCursorRes
        Left errMsg->do
            putStrLn errMsg
            iPlacePawnUntilCorrect handle (WholeState wb ps (y, x) b ws)

updateEnemyPawn :: Handle -> WholeState -> IO (Either String WholeState)
updateEnemyPawn handle ws@(WholeState _ _ _ flag _) =
  if flag
    then return $ Right ws
    else do
      (y, x) <- getEnemyPawn handle
      return (enemyPlacePawn ws (y, x))

initBoard::Handle->IO WholeState
initBoard handle = do
    sideMsg <- hGetLine handle
    let isPlayerBlack = sideMsg == "You are Black"
    let size = 10
    let playCh = if isPlayerBlack then 'b' else 'w'
    let wholeState = initWholeState size playCh
    return wholeState
