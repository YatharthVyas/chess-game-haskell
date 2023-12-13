module Server where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Control.Concurrent (forkIO)
import System.Environment (getArgs)
import Control.Exception (try, catch, bracketOnError, finally ) 
import System.IO.Error ( catchIOError, isAlreadyInUseError )

createServer :: IO Socket
createServer = withSocketsDo $ do
    -- args <- getArgs
    -- let port = head args
    let port = "8080"
    addr <- resolve port
    sock <- open addr
    -- return the connection object as this is what the server uses to receive and send data
    (conn, _) <- accept sock
    return conn

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
    listen sock 2
    return sock
