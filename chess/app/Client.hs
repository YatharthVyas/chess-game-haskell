module Client where
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C

connectToServer :: IO Socket
connectToServer = withSocketsDo $ do
    -- Replace "localhost" with the server's address and "8080" with the server's port
    addr <- resolve "localhost" "8080"
    sock <- open addr
    -- Return just the socket because this is what the client uses to receive and send data
    return sock

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) (Just host) (Just port)

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    return sock
