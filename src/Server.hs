module Server (startServer) where

import Network.Socket
import Control.Concurrent
import Control.Monad (forever, when)
import System.IO
import Data.List (intercalate)
import Data.List.Split (splitOn)

import Logic(BoardDotStat(..),boardDotStatCons,
             WholeBoard(..),initWholeBoard,getDotStat,WholeState(..),initWholeState,getCorrespondentPawn,
             moveCursor,iPlacePawn,placePawnAtCursor,enemyPlacePawn,placePawn,fetchWholeState, PlayerSide, WinStat)


type Message = String
type Coordinate = (Int, Int)

startServer :: IO ()
startServer = withSocketsDo $ do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 2  -- 监听两个连接
    putStrLn "Server is running on port 3000"

    (blackSock, _) <- accept sock
    blackHandle <- socketToHandle blackSock ReadWriteMode
    hSetBuffering blackHandle NoBuffering

    (whiteSock, _) <- accept sock
    whiteHandle <- socketToHandle whiteSock ReadWriteMode
    hSetBuffering whiteHandle NoBuffering

    handleClients blackHandle whiteHandle
    hClose blackHandle
    hClose whiteHandle
    close sock

handleClients :: Handle -> Handle -> IO ()
handleClients blackHandle whiteHandle = do
    hPutStrLn blackHandle "Enter the size of the board:"
    sizeStr <- hGetLine blackHandle
    let size = read sizeStr :: Int
    let whState = initWholeState size 'b'

    gameLoop blackHandle whiteHandle (Right whState) True

gameLoop :: Handle -> Handle -> Either String WholeState -> Bool -> IO ()
gameLoop blackHandle whiteHandle (Right newState) isMyTurn = do
    let currentHandle = if isMyTurn then blackHandle else whiteHandle
    hPutStrLn currentHandle "Send your move (format: x,y):"
    coordStr <- hGetLine currentHandle

    when (coordStr /= "quit") $ do
        let coord = parseCoordinate coordStr
        let eitherNewState = placePawn newState coord isMyTurn

        case eitherNewState of
            Right newState' -> do
                putStrLn $ "Received coordinate: " ++ show coord
                putStrLn $ "Updated board state: \n" ++ show (fetchWholeState (Right newState'))
                hPutStrLn blackHandle $ "Updated board state: \n" ++ serializeWholeState (fetchWholeState (Right newState'))
                hPutStrLn whiteHandle $ "Updated board state: \n" ++ serializeWholeState (fetchWholeState (Right newState'))
                putStrLn $ "Serialized board state: \n" ++ serializeWholeState (fetchWholeState (Right newState'))
                gameLoop blackHandle whiteHandle (Right newState') (not isMyTurn)
            Left errorMsg -> do
                hPutStrLn currentHandle $ "Error: " ++ errorMsg
                gameLoop blackHandle whiteHandle (Right newState) isMyTurn

gameLoop _ _ (Left errorMsg) _ = putStrLn $ "Error: " ++ errorMsg

parseCoordinate :: String -> Coordinate
parseCoordinate s = let [x, y] = map read $ splitOn "," s
                    in (x, y)



serializeWholeState :: WholeState -> String
serializeWholeState (WholeState board player (x, y) flag winStat) =
    serializeWholeBoard board ++ ";" ++
    show player ++ ";" ++
    show x ++ "," ++ show y ++ ";" ++
    show flag ++ ";" ++
    show winStat

serializeBoardDotStat :: BoardDotStat -> String
serializeBoardDotStat BlackPawn = "1"
serializeBoardDotStat WhitePawn = "2"
serializeBoardDotStat EmptyDot = "3"

serializeWholeBoard :: WholeBoard -> String
serializeWholeBoard (WholeBoard size board) =
    show size ++ ";" ++ intercalate "," (concatMap (map serializeBoardDotStat) board)




