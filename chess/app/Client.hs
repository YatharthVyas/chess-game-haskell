module Client where
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C

connectToHost :: IO Socket
connectToHost = withSocketsDo $ do
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