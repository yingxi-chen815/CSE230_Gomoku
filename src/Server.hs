module Server (startServer) where

import Network.Socket
import System.IO
import Control.Monad (forever)
import Control.Concurrent (forkIO)

type Coordinate = (Int, Int)

startServer :: IO ()
startServer = withSocketsDo $ do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "3000")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 2
    putStrLn "Server is running on port 3000"

    (blackSock, _) <- accept sock
    putStrLn "Black player connected"
    blackHandle <- socketToHandle blackSock ReadWriteMode
    hSetBuffering blackHandle NoBuffering
    hPutStrLn blackHandle "You are Black"

    (whiteSock, _) <- accept sock
    putStrLn "White player connected"
    whiteHandle <- socketToHandle whiteSock ReadWriteMode
    hSetBuffering whiteHandle NoBuffering
    hPutStrLn whiteHandle "You are White"

    _ <- forkIO $ handleClient blackHandle whiteHandle
    handleClient whiteHandle blackHandle

    hClose blackHandle
    hClose whiteHandle
    close sock

handleClient :: Handle -> Handle -> IO ()
handleClient fromHandle toHandle = forever $ do
    move <- hGetLine fromHandle
    hPutStrLn toHandle move