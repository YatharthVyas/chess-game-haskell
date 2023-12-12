module Client where
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Data.Traversable (for)
import Control.Monad (forever)

main :: IO ()
main = withSocketsDo $ do
    -- Replace "localhost" with the server's address and "8080" with the server's port
    addr <- resolve "localhost" "8080"
    sock <- open addr
    putStrLn "Connected to server"
    sendAll sock (C.pack "Hello, server!")
    msg <- recv sock 1024
    putStrLn $ "Received: " ++ C.unpack msg
    close sock

connectToServer :: IO ()
connectToServer = withSocketsDo $ do
    -- Replace "localhost" with the server's address and "8080" with the server's port
    addr <- resolve "localhost" "8080"
    sock <- open addr
    putStrLn "Connected to server"
    sendAll sock (C.pack "Hello, server!")
    -- Keep listening for messages from the server
    forever $ do
        msg <- recv sock 1024
        putStrLn $ "Received: " ++ C.unpack msg
        if C.unpack msg == "quit" then do
            putStrLn "Closing connection"
            close sock
            return ()
        else do
            putStrLn "Enter a message to send to the server"
            msg <- getLine
            sendAll sock (C.pack msg)
    close sock

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) (Just host) (Just port)

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    return sock
