module Server where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Control.Concurrent (forkIO)
import System.Environment (getArgs)
import Control.Exception (try, catch, bracketOnError, finally ) 
import System.IO.Error ( catchIOError, isAlreadyInUseError )

createServer :: IO ()
createServer = withSocketsDo $ do
    -- args <- getArgs
    -- let port = head args
    let port = "8080"
    addr <- resolve port
    sock <- open addr
    putStrLn $ "Listening on " ++ port
    (conn, _) <- accept sock
    forkIO $ handleConn conn
    loop sock
    finally (loop sock) (close sock)

isPortAvailable :: String -> IO Bool
isPortAvailable port = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
    result <- try $ bracketOnError (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
                                   close  -- Always close the socket in case of error
                                   (\sock -> do
                                        bind sock (addrAddress addr)
                                        close sock)
    case result of
        Left e -> if isAlreadyInUseError e then return False else ioError e
        Right _ -> return True

resolve :: String -> IO AddrInfo
resolve port = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) Nothing (Just port)

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 10
    return sock

loop :: Socket -> IO ()
loop sock = do
    (conn, _) <- accept sock
    forkIO $ handleConn conn
    loop sock

handleConn :: Socket -> IO ()
handleConn conn = do
    msg <- recv conn 1024
    if C.null msg
        then return ()
        else do
            sendAll conn (C.pack "Wassup!\n")
            handleConn conn
