{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | YMDNS protocol implementation, all those multicast-related
-- things.

module Resolving where

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.STM.Delay (Delay, newDelay, updateDelay, waitDelay)
import           Control.Lens                 (makeLenses, (%=))
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Internal     as BS (c2w)
import qualified Data.Map.Strict              as M
import           Data.Store                   (Size (..), Store (..), decode, encode)
import qualified GHC.Show                     (show)
import           Network.BSD                  (getProtocolNumber)
import           Network.Multicast            (multicastReceiver)
import           Network.Socket               hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           System.Timeout               (timeout)
import           Universum                    hiding (ByteString)

----------------------------------------------------------------------------
-- Network message types
----------------------------------------------------------------------------

-- | Hostname we can reserve. Should be <256 chars, ascii low-letters only.
newtype Hostname = Hostname
    { getHostname :: String
    } deriving (Show)

checkHostnameChar :: (MonadFail m) => Word8 -> m ()
checkHostnameChar c =
    unless (or
            [ c >= BS.c2w 'a' && c <= BS.c2w 'z'
            , c >= BS.c2w '0' && c <= BS.c2w '9'
            , c == BS.c2w '.'
            ]) $
        fail $ "Hostname character not in range [(a..z)|(0..9)|.]: '" <>
               [chr $ fromIntegral c] <> "'"

createHostname :: (MonadFail m) => String -> m Hostname
createHostname s = do
    when (length s > 256) $
        fail "Hostname shouldn't be more than 256 elements long"
    mapM_ (checkHostnameChar . BS.c2w) s
    pure $ Hostname s


data InetAddress = InetAddress
    { ia_address :: Word32
    , ia_port    :: Word16
    } deriving (Eq, Ord)

instance Show InetAddress where
    show (InetAddress addr port) = concat
        [ hostAddressToTuple addr & \(a,b,c,d) -> intercalate "." (map show [a,b,c,d])
        , ":"
        , show port
        ]

inetAddressToSockAddr :: InetAddress -> SockAddr
inetAddressToSockAddr (InetAddress addr port) = SockAddrInet (fromIntegral port) addr

sockAddrToInetAddress :: SockAddr -> InetAddress
sockAddrToInetAddress (SockAddrInet port addr) = InetAddress addr (fromIntegral port)
sockAddrToInetAddress _  = panic "sockAddrToInetAddress: not ipv4 address"


-- | Resolve map from hostname to ip.
data ResolveMap = ResolveMap
    { ownerHost     :: Hostname
    , getResolveMap :: [(Hostname, InetAddress)]
    } deriving (Show, Generic)

addHostname :: Hostname -> InetAddress -> ResolveMap -> ResolveMap
addHostname host addr (ResolveMap oh other) = ResolveMap oh ((host,addr):other)

-- new ResolveMap (when we are the first node in the network)
newResolveMap :: Hostname -> ResolveMap
newResolveMap myHostname = ResolveMap myHostname []

-- convert received ResolveMap
adaptResolveMap :: Hostname -> InetAddress -> ResolveMap -> ResolveMap
adaptResolveMap myHostname senderAddress (ResolveMap senderHost other)
    = ResolveMap myHostname ((senderHost, senderAddress) : other)

resolveMapRemove :: InetAddress -> ResolveMap -> ResolveMap
resolveMapRemove address (ResolveMap oh other)
    = ResolveMap oh $ filter (\(_host, address') -> address' /= address) other


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

instance Store InetAddress where
    size = ConstSize 6
    poke (InetAddress addr port) = poke addr >> poke port
    peek = InetAddress <$> peek <*> peek

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
    size = VarSize $ \(ResolveMap (Hostname oh) xs) ->
            (1 + length oh) + 1 + sum (map (\(Hostname h,_) -> (1 + length h) + 6) xs)
    poke (ResolveMap oh xs) = do
        when (length xs > 256) $
            fail "Resolve map length shouldn't be more than 256 elements long"
        poke oh
        poke (fromIntegral (length xs) :: Word8)
        forM_ xs $ \(h,ip) -> poke h >> poke ip
    peek = do
        (oh :: Hostname) <- peek
        (l :: Word8) <- peek
        fmap (ResolveMap oh) . replicateM (fromIntegral l) $
            (,) <$> peek <*> peek

-- Using generics here s licom mrazi
instance Store YMDnsMsg

----------------------------------------------------------------------------
-- Generic utility functions
----------------------------------------------------------------------------

-- retry computation until it returns Just
retryM :: (Monad m) => m (Maybe a) -> m a
retryM action =
  action >>= \case
    Just a -> return a
    Nothing -> retryM action

-- at most n times
retryMN :: (Monad m) => Int -> m (Maybe a) -> m (Maybe a)
retryMN n _      | n < 0 = panic "retryM: negative"
retryMN 0 _      = return Nothing
retryMN n action =
  action >>= \case
    Just a -> return (Just a)
    Nothing -> retryMN (n-1) action

seconds :: Int -> Int
seconds k = k * 10^(6::Int)

----------------------------------------------------------------------------
-- Protocol helpers
----------------------------------------------------------------------------

createUdpSocket :: IO Socket
createUdpSocket = socket AF_INET Datagram =<< getProtocolNumber "udp"

createMulticastSocket :: IO Socket
createMulticastSocket = multicastReceiver "224.0.0.250" 5565

multicastAddress :: InetAddress
multicastAddress = InetAddress (tupleToHostAddress (224,0,0,250)) 5565

sendMsgTo :: (MonadIO m, MonadFail m) => Socket -> InetAddress -> YMDnsMsg -> m ()
sendMsgTo sock addr msg = do
    let bs = encode msg
        len = BS.length bs
    n <- liftIO $ sendTo sock bs (inetAddressToSockAddr addr)
    when (n /= len) $
        fail "Couldn't send an msg via one package"

recvMsgFrom :: (MonadIO m, MonadFail m) => Socket -> m (YMDnsMsg, InetAddress)
recvMsgFrom sock = do
    let maxSize = 65536
    (encoded, sockAddr) <- liftIO $ recvFrom sock maxSize
    let addr = sockAddrToInetAddress sockAddr
    case decode encoded of
      Left e    -> fail $ "decoding message failed: " <> show e
      Right msg -> do
        putText $ "message from " <> show addr <> ": "<> show msg
        return (msg, addr)

----------------------------------------------------------------------------
-- Worker, server part
----------------------------------------------------------------------------

data YMDnsEvent
    = MulticastMessage InetAddress YMDnsMsg
    | ResendHeartbeat
    | HeartbeatTimeout InetAddress

data YMDnsConfig = YMDnsConfig
  { unicastSocket   :: Socket
  , multicastSocket :: Socket
  , eventChannel    :: Chan YMDnsEvent
  }

data YMDnsState = YMDnsState
  { _lResolveMap           :: ResolveMap
  , _lHeartbeatTimers :: M.Map InetAddress Delay
  }

makeLenses ''YMDnsState

resendHeartbeatInterval, heartbeatTimeout :: Int
resendHeartbeatInterval = seconds 3
heartbeatTimeout = seconds 10

startHeartbeatTimer :: (MonadIO m) => Chan YMDnsEvent -> InetAddress -> m Delay
startHeartbeatTimer eventChannel address = liftIO $ do
    timer <- newDelay heartbeatTimeout
    _ <- forkIO $ do
        atomically $ waitDelay timer
        writeChan eventChannel (HeartbeatTimeout address)
    return timer

scheduleNextHeartbeat :: Chan YMDnsEvent -> IO ()
scheduleNextHeartbeat eventChannel = void $ forkIO $ do
    threadDelay resendHeartbeatInterval
    writeChan eventChannel ResendHeartbeat

startMulticastListener :: Chan YMDnsEvent -> Socket -> IO ()
startMulticastListener eventChannel multicastSocket = do
    void $ forkIO $ forever $ do
        (msg, sender) <- recvMsgFrom multicastSocket
        writeChan eventChannel (MulticastMessage sender msg)


-- | YMDns server, blocks.
resolveWorker :: Hostname -> IO ()
resolveWorker host = producerAction host `catch` handler
  where
    handler (e :: SomeException) = do
        putText $ "Exception happened in resolveWorker: " <> show e
        resolveWorker host

-- TODO
shouldWeAnswer :: ResolveMap -> Hostname -> Bool
shouldWeAnswer _resolveMap _requesterHost = True

producerAction :: Hostname -> IO ()
producerAction myHostname = withSocketsDo $ do
    unicastSocket <- createUdpSocket

    let joinResendInterval = seconds 1
        joinRetries = 3

    {- joining and getting initial ResolveMap:
       - send Join
       - wait for the node map
       - if no response resend Join several times
       - if still no response then there are probably no other nodes
    -}
    initialResolveMap <-
        fmap (fromMaybe $ newResolveMap myHostname) $
        retryMN joinRetries $ do
          putText "sending Join"
          sendMsgTo unicastSocket multicastAddress $ YMDnsJoin myHostname
          timeout joinResendInterval $ do
            retryM $ do
              (msg, sender) <- recvMsgFrom unicastSocket
              case msg of
                  YMDnsShare nodeMap ->
                    return $ Just $ adaptResolveMap myHostname sender nodeMap
                  _ -> return Nothing  -- ignore other messages
    putText $ "initial map: " <> show initialResolveMap

    multicastSocket <- createMulticastSocket

    eventChannel <- newChan @ YMDnsEvent
    initialHeartbeatTimers <- M.fromList <$> do
        forM (getResolveMap initialResolveMap) $ \(_host, addr) -> do
            (,) addr <$> startHeartbeatTimer eventChannel addr
    startMulticastListener eventChannel multicastSocket
    scheduleNextHeartbeat eventChannel

    let initialState = YMDnsState initialResolveMap initialHeartbeatTimers
    let config = YMDnsConfig{unicastSocket, multicastSocket, eventChannel}

    -- main loop
    void $ flip runStateT initialState $
      forever $ do
        event <- liftIO $ readChan eventChannel
        handleEvent config event

handleEvent :: YMDnsConfig -> YMDnsEvent -> StateT YMDnsState IO ()
handleEvent YMDnsConfig{..} event = case event of
    MulticastMessage sender (YMDnsJoin senderHost) -> do
        use lResolveMap >>= \resolveMap ->
            when (shouldWeAnswer resolveMap senderHost) $
                sendMsgTo unicastSocket sender $ YMDnsShare resolveMap
        lResolveMap %= addHostname senderHost sender
        startHeartbeatTimer eventChannel sender >>= \timer ->
            lHeartbeatTimers %= M.insert sender timer
        use lResolveMap >>= \m -> putText ("current map: " <> show m)
    MulticastMessage sender YMDnsHeartbeat -> do
        M.lookup sender <$> use lHeartbeatTimers >>= \case
            Nothing    -> return () -- either our own heartbeat or an unknown node
            Just timer -> liftIO $ updateDelay timer heartbeatTimeout
    MulticastMessage _ _ -> do
        fail "unexpected multicast message"
    ResendHeartbeat -> do
        putText $ "sending heartbeat"
        sendMsgTo unicastSocket multicastAddress YMDnsHeartbeat
        liftIO $ scheduleNextHeartbeat eventChannel
    HeartbeatTimeout address -> do
        putText $ "node timed out: " <> show address
        lResolveMap %= resolveMapRemove address
        lHeartbeatTimers %= M.delete address
        use lResolveMap >>= \m -> putText ("current map: " <> show m)

-- | Function for spawning a producer in another thread
serveProducer :: String -> IO ()
serveProducer host = void $ forkIO $ resolveWorker $ Hostname host

-----------------------------------------------------------------------------
-- Worker, client part
-----------------------------------------------------------------------------

sendRequest :: String -> IO ()
sendRequest host = withSocketsDo $ do
    sock <- createUdpSocket
    let msg = YMDnsRequest $ Hostname host
    sendMsgTo sock multicastAddress msg

-- TODO: how to do it nicely?
waitForResponse :: IO ResolveMap
waitForResponse = notImplemented

downloadFile :: ResolveMap -> IO ()
downloadFile _ = putText "Haha, loh, failov net"
