module Main where

import qualified Data.ByteString           as BS
import           Data.Store                (Store (..), encode)
import           Network.Multicast
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           Universum

import           Options
import           Resolving

multicastHost :: HostName
multicastHost = "224.0.0.250"

multicastPort :: PortNumber
multicastPort = 5565

sendMsgTo :: Store msg => Socket -> SockAddr -> msg -> IO ()
sendMsgTo sock addr msg = do
    let bs = encode msg
        len = BS.length bs
    n <- sendTo sock bs addr
    when (n /= len) $
        fail "Couldn't send an msg via one package"

sendRequest :: String -> IO ()
sendRequest host = do
    (sock, addr) <- multicastSender multicastHost multicastPort
    let msg = YMDnsRequest $ Hostname host
    sendMsgTo sock addr msg

waitForResponse :: IO ResolveMap
waitForResponse = return $ ResolveMap []

downloadFile :: ResolveMap -> IO ()
downloadFile _ = putText "Haha, loh, failov net"

serveProducer :: String -> Int -> FilePath -> IO ()
serveProducer host port files = do
    sock <- multicastReceiver multicastHost multicastPort
    let loop = do
            (msg, addr) <- recvFrom sock 65536
            print (msg, addr)
    loop

main :: IO ()
main = withSocketsDo $ do
    o <- getOptions
    putText $ "Launched with opts: " <> show o
    case action o of
        YMRequest req -> sendRequest req >> waitForResponse >>= downloadFile
        YMServe {..}  -> serveProducer hostname port filesDir
