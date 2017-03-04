{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | YMDNS protocol implementation, all those multicast-related
-- things.

module Resolving where

import           Control.Concurrent        (forkIO)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Internal  as BS (c2w)
import           Data.Store                (Size (..), Store (..), encode)
import           Network.Multicast
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           Universum                 hiding (ByteString)

----------------------------------------------------------------------------
-- Network message types
----------------------------------------------------------------------------

-- | Hostname we can reserve. Should be <256 chars, ascii low-letters only.
newtype Hostname = Hostname
    { getHostname :: String
    } deriving (Show)

checkHostnameChar :: (MonadFail m) => Word8 -> m ()
checkHostnameChar c =
    unless ((c >= 97 && c <= 122) || c == 46) $
        fail $ "Hostname character not in range [(a..z)|.]: '" <>
               [chr $ fromIntegral c] <> "'"

createHostname :: (MonadFail m) => String -> m Hostname
createHostname s = do
    when (length s > 256) $
        fail "Hostname shouldn't be more than 256 elements long"
    mapM_ (checkHostnameChar . BS.c2w) s
    pure $ Hostname s

-- | Resolve map from hostname to ip.
newtype ResolveMap = ResolveMap
    { getResolveMap :: [(Hostname, Int32)]
    } deriving (Show, Generic)

-- | YMDns message type
data YMDnsMsg
    = YMDnsJoin { ymdJoinHostname :: Hostname}
    | YMDnsShare { ymdShared :: ResolveMap }
    | YMDnsRequest { ymdReq :: Hostname}
    | YMDnsResponse { ymdResp :: ResolveMap }
    | YMDnsHeartbeat
    deriving (Show, Generic)

----------------------------------------------------------------------------
-- Binary serialization
----------------------------------------------------------------------------

instance Store Hostname where
    size = VarSize $ \(Hostname s) -> length s + 1
    poke (Hostname s) = do
        when (length s > 256) $
            fail "Hostname shouldn't be more than 256 elements long"
        poke (fromIntegral (length s) :: Word8)
        forM_ s $ \(c :: Char) -> do
            let c' = fromIntegral $ ord c
            checkHostnameChar c'
            poke (c' :: Word8)
    peek = do
        (l :: Word8) <- peek
        fmap Hostname . replicateM (fromIntegral l) $ do
            (c' :: Word8) <- peek
            checkHostnameChar c'
            pure $ chr $ fromIntegral c'

instance Store ResolveMap where
    size = VarSize $ \(ResolveMap xs) ->
            1 + sum (map (\(Hostname h,_) -> length h + 5) xs)
    poke (ResolveMap xs) = do
        when (length xs > 256) $
            fail "Resolve map length shouldn't be more than 256 elements long"
        poke (fromIntegral (length xs) :: Word)
        forM_ xs $ \(h,ip) -> poke h >> poke ip
    peek = do
        (l :: Word8) <- peek
        fmap ResolveMap . replicateM (fromIntegral l) $
            (,) <$> peek <*> peek

-- Using generics here s licom mrazi
instance Store YMDnsMsg

----------------------------------------------------------------------------
-- Protocol constants
----------------------------------------------------------------------------

multicastHost :: HostName
multicastHost = "224.0.0.250"

multicastPort :: PortNumber
multicastPort = 5565

----------------------------------------------------------------------------
-- Protocol helpers
----------------------------------------------------------------------------

sendMsgTo :: Store msg => Socket -> SockAddr -> msg -> IO ()
sendMsgTo sock addr msg = do
    let bs = encode msg
        len = BS.length bs
    n <- sendTo sock bs addr
    when (n /= len) $
        fail "Couldn't send an msg via one package"

----------------------------------------------------------------------------
-- Worker, server part
----------------------------------------------------------------------------

data YMDnsState = YMDnsState
    { neighborsAddress :: Map Hostname HostName
    , ourHostname      :: Hostname
    }

initYMDns :: Hostname -> YMDnsState
initYMDns = undefined

-- | YMDns server, blocks.
resolveWorker :: Hostname -> IO ()
resolveWorker host = producerAction host `catch` handler
  where
    handler (e :: SomeException) = do
        putText $ "Exception happened in resolveWorker: " <> show e
        resolveWorker host

producerAction :: Hostname -> IO ()
producerAction host = withSocketsDo $ do
    sock <- multicastReceiver multicastHost multicastPort
    let loop = do
            (msg, addr) <- recvFrom sock 65536
            print (msg, addr)
    loop

-- | Function for spawning a producer in another thread
serveProducer :: String -> IO ()
serveProducer host = void $ forkIO $ resolveWorker $ Hostname host

-----------------------------------------------------------------------------
-- Worker, client part
-----------------------------------------------------------------------------

sendRequest :: String -> IO ()
sendRequest host = withSocketsDo $ do
    (sock, addr) <- multicastSender multicastHost multicastPort
    let msg = YMDnsRequest $ Hostname host
    sendMsgTo sock addr msg

-- TODO: how to do it nicely?
waitForResponse :: IO ResolveMap
waitForResponse = return $ ResolveMap []

downloadFile :: ResolveMap -> IO ()
downloadFile _ = putText "Haha, loh, failov net"
