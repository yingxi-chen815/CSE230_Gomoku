module Client(startClient) where

import Network.Socket
import System.IO
import Control.Monad (when, unless)

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
