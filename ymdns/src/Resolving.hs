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
import           Control.Lens                 (makeLenses, (%=), (+=), (-=))
import           Criterion.Measurement        (getCPUTime, initializeTime)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Internal     as BS (c2w)
import           Data.Hashable                (hash)
import           Data.List                    (last, lookup)
import qualified Data.Map.Strict              as M
import           Data.Store                   (Size (..), Store (..), decode, encode)
import qualified GHC.Show                     (show)
import           Network.BSD                  (getProtocolNumber)
import           Network.Multicast            (multicastReceiver)
import           Network.Socket               (Family (AF_INET), SockAddr (SockAddrInet),
                                               Socket, SocketType (Datagram),
                                               hostAddressToTuple, socket,
                                               tupleToHostAddress, withSocketsDo)
import           Network.Socket.ByteString    (recvFrom, sendTo)
import           System.Timeout               (timeout)
import           Universum                    hiding (ByteString)

----------------------------------------------------------------------------
-- Network message types
----------------------------------------------------------------------------

-- | Hostname we can reserve. Should be <256 chars, ascii low-letters only.
newtype Hostname = Hostname
    { getHostname :: String
    } deriving (Show, Eq, Ord, Hashable)

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

showHostAddress :: Word32 -> String
showHostAddress addr = hostAddressToTuple addr & \(a,b,c,d) -> intercalate "." (map show [a,b,c,d])

instance Show InetAddress where
    show (InetAddress addr port) = showHostAddress addr <> ":" <> show port

inetAddressToSockAddr :: InetAddress -> SockAddr
inetAddressToSockAddr (InetAddress addr port) = SockAddrInet (fromIntegral port) addr

sockAddrToInetAddress :: SockAddr -> InetAddress
sockAddrToInetAddress (SockAddrInet port addr) = InetAddress addr (fromIntegral port)
sockAddrToInetAddress _  = panic "sockAddrToInetAddress: not ipv4 address"

-- | Resolve map from hostname to ip.
data ResolveMap = ResolveMap
    { _ownerHost     :: Hostname
    , _ownerLoad     :: Word8
    , _getResolveMap :: [(Hostname, (InetAddress, Word8))]
    } deriving (Show, Generic)

makeLenses ''ResolveMap

addHostname :: Hostname -> InetAddress -> Word8 -> ResolveMap -> ResolveMap
addHostname host addr load (ResolveMap oh ol other) = ResolveMap oh ol ((host,(addr, load)):other)

-- new ResolveMap (when we are the first node in the network)
newResolveMap :: Hostname -> Word8 -> ResolveMap
newResolveMap myHostname myLoad = ResolveMap myHostname myLoad []

-- convert received ResolveMap
adaptResolveMap :: Hostname -> InetAddress -> Word8 -> ResolveMap -> ResolveMap
adaptResolveMap myHostname senderAddress load (ResolveMap senderHost senderLoad other)
    = ResolveMap myHostname load ((senderHost, (senderAddress, senderLoad)) : other)

resolveMapRemove :: InetAddress -> ResolveMap -> ResolveMap
resolveMapRemove address (ResolveMap oh ol other)
    = ResolveMap oh ol $ filter (\(_host, (address', _)) -> address' /= address) other

data ResolveResult
    = NotFound
    | Owner
    | Found InetAddress
    deriving (Show, Generic)

resolve :: ResolveMap -> Hostname -> ResolveResult
resolve (ResolveMap ownerHost' _ _) reqHost | ownerHost' == reqHost = Owner
resolve (ResolveMap _ _ other) reqHost = case lookup reqHost other of
    Nothing        -> NotFound
    Just (addr, _) -> Found addr

newtype Task = Task Int deriving (Show, Generic, Store)
data TaskResponse = TaskResponse Int Text deriving (Show, Generic)

-- | YMDns message type
data YMDnsMsg
    = YMDnsJoin { ymdJoinHostname :: Hostname}
    | YMDnsShare { ymdShared :: ResolveMap }
    | YMDnsRequest { ymdReq :: Hostname }
    | YMDnsResponse { ymdResp :: ResolveResult }
    | YMDnsTask { ymdTask :: Task }
    | YMDnsTaskResponse { ymdTaskResponse :: Maybe TaskResponse }
    | YMDnsHeartbeat { ymdLoad :: Word8 }
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
    size = VarSize $ \(ResolveMap (Hostname oh) ol xs) ->
            (1 + length oh) + 1 + 1 + sum (map (\(Hostname h,_) -> (1 + length h) + 6 + 1) xs)
    poke (ResolveMap oh ol xs) = do
        when (length xs > 256) $
            fail "Resolve map length shouldn't be more than 256 elements long"
        poke oh
        poke ol
        poke (fromIntegral (length xs) :: Word8)
        forM_ xs $ \(h,(ip,l)) -> poke h >> poke ip >> poke l
    peek = do
        (oh :: Hostname) <- peek
        (ol :: Word8) <- peek
        (l :: Word8) <- peek
        fmap (ResolveMap oh ol) . replicateM (fromIntegral l) $ do
            host <- peek
            addr <- peek
            load <- peek
            pure (host, (addr, load))

-- Using generics here s licom mrazi
instance Store ResolveResult
instance Store TaskResponse
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

timeComputation :: IO a -> IO (Int, a)
timeComputation action = do
    initializeTime
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    return (round (end - start) * 1000, result)

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
    | TaskFinished

data YMDnsConfig = YMDnsConfig
  { unicastSocket   :: Socket
  , multicastSocket :: Socket
  , kNeighbors      :: Int
  , eventChannel    :: Chan YMDnsEvent
  }

data YMDnsState = YMDnsState
  { _lResolveMap      :: ResolveMap
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
resolveWorker :: Hostname -> Int -> IO ()
resolveWorker host kNeighbors =
    producerAction host kNeighbors `catch` handler
  where
    handler (e :: SomeException) = do
        putText $ "Exception happened in resolveWorker: " <> show e
        resolveWorker host kNeighbors

shouldWeAnswer :: ResolveMap -> Int -> Hostname -> Bool
shouldWeAnswer (ResolveMap myHost _ other) k reqHost =
    if | myHost == reqHost -> True
       | otherwise -> null otherHosts || dist myHost < fst pastKNeighbor
  where
    pastKNeighbor =
        case drop k searchList of
            (x:_) -> x
            []    -> last searchList
    searchList = sortOn fst $ map (\x -> (dist x,x)) otherHosts

    otherHosts :: [Hostname]
    otherHosts = map (\(host, _addr) -> host) other

    dist :: Hostname -> Int
    dist host = abs $ hash host - hash reqHost

data ShouldExecuteRes = SRExecute | SRPass | SRFail

shouldWeExecute :: ResolveMap -> ShouldExecuteRes
shouldWeExecute resmap
    | null otherNodes || ourpair < minWhatever =
      if resmap ^. ownerLoad < maxLoadFactor then SRExecute else SRFail
    | otherwise = SRPass
  where
    ourpair = (resmap ^. ownerLoad, resmap ^. ownerHost)
    otherNodes = view getResolveMap resmap
    minWhatever = minimum $ map (\(h,(_,w)) -> (w,h)) $ otherNodes
    maxLoadFactor = 2

producerAction :: Hostname -> Int -> IO ()
producerAction myHostname kNeighbors = withSocketsDo $ do
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
        fmap (fromMaybe $ newResolveMap myHostname 0) $
        retryMN joinRetries $ do
          putText "sending Join"
          sendMsgTo unicastSocket multicastAddress $ YMDnsJoin myHostname
          timeout joinResendInterval $ do
            retryM $ do
              (msg, sender) <- recvMsgFrom unicastSocket
              case msg of
                  YMDnsShare nodeMap ->
                    return $ Just $ adaptResolveMap myHostname sender 0 nodeMap
                  _ -> return Nothing  -- ignore other messages
    putText $ "initial map: " <> show initialResolveMap

    multicastSocket <- createMulticastSocket

    eventChannel <- newChan @ YMDnsEvent

    initialHeartbeatTimers <- M.fromList <$> do
        forM (_getResolveMap initialResolveMap) $ \(_host, (addr, _load)) -> do
            (,) addr <$> startHeartbeatTimer eventChannel addr

    startMulticastListener eventChannel multicastSocket
    scheduleNextHeartbeat eventChannel

    let initialState = YMDnsState initialResolveMap initialHeartbeatTimers
    let config = YMDnsConfig{..}
    -- unicastSocket
    --                         , multicastSocket
    --                         , eventChannel
    --                         , 4}

    -- main loop
    void $ flip runStateT initialState $
      forever $ do
        event <- liftIO $ readChan eventChannel
        handleEvent config event

handleEvent :: YMDnsConfig -> YMDnsEvent -> StateT YMDnsState IO ()
handleEvent YMDnsConfig{..} event = case event of
    MulticastMessage sender (YMDnsJoin senderHost) -> do
        use lResolveMap >>= \resolveMap ->
            when (shouldWeAnswer resolveMap kNeighbors senderHost) $
                sendMsgTo unicastSocket sender $ YMDnsShare resolveMap
        lResolveMap %= addHostname senderHost sender 0
        startHeartbeatTimer eventChannel sender >>= \timer ->
            lHeartbeatTimers %= M.insert sender timer
        use lResolveMap >>= \m -> putText ("current map: " <> show m)
    MulticastMessage sender (YMDnsHeartbeat load) -> do
        M.lookup sender <$> use lHeartbeatTimers >>= \case
            Nothing    -> return () -- either our own heartbeat or an unknown node
            Just timer -> do
                liftIO $ updateDelay timer heartbeatTimeout
                let mapper a@(host, (addr, _))
                        | addr == sender = (host, (addr, load))
                        | otherwise = a
                lResolveMap . getResolveMap %= map mapper
    MulticastMessage sender (YMDnsRequest requestedHost) -> do
        resolveMap <- use lResolveMap
        when (shouldWeAnswer resolveMap kNeighbors requestedHost) $ do
            let result = resolve resolveMap requestedHost
            putText $ "resolving result: " <> show result
            putText $ "answering to " <> show sender
            sendMsgTo unicastSocket sender $ YMDnsResponse result
            putText "answered to request"
    MulticastMessage sender (YMDnsTask (Task delay)) -> do
        resolveMap <- use lResolveMap
        case shouldWeExecute resolveMap of
            SRExecute -> do
                lResolveMap . ownerLoad += 1
                use (lResolveMap . ownerLoad) >>= \ownLoad ->
                    -- resend heartbeat
                    sendMsgTo unicastSocket multicastAddress $ YMDnsHeartbeat ownLoad
                void . liftIO . forkIO $ do
                    putText "Execution started"
                    (time, result) <- timeComputation $ do
                      threadDelay (seconds delay)
                      return "blah"
                    putText "Execution done, sending message"
                    sendMsgTo unicastSocket sender $
                        YMDnsTaskResponse $ Just $ TaskResponse time result
                    writeChan eventChannel TaskFinished
            SRPass -> pass
            SRFail -> sendMsgTo unicastSocket sender $ YMDnsTaskResponse Nothing
    MulticastMessage _ _ -> do
        fail "unexpected multicast message"
    ResendHeartbeat -> do
        putText $ "sending heartbeat"
        ownLoad <- use $ lResolveMap . ownerLoad
        sendMsgTo unicastSocket multicastAddress $ YMDnsHeartbeat ownLoad
        liftIO $ scheduleNextHeartbeat eventChannel
    HeartbeatTimeout address -> do
        putText $ "node timed out: " <> show address
        lResolveMap %= resolveMapRemove address
        lHeartbeatTimers %= M.delete address
        use lResolveMap >>= \m -> putText ("current map: " <> show m)
    TaskFinished -> do
        lResolveMap . ownerLoad -= 1

-- | Function for spawning a producer in another thread
serveProducer :: String -> Int -> IO ()
serveProducer host kNeighbors =
    void $ forkIO $ resolveWorker (Hostname host) kNeighbors

-----------------------------------------------------------------------------
-- Worker, client part
-----------------------------------------------------------------------------

resolveHost :: Hostname -> IO (Maybe InetAddress)
resolveHost host = withSocketsDo $ do
    udpSocket <- createUdpSocket

    let resendInterval = seconds 3
    (resolveResult, sender) <- retryM $ do
        putText "sending request"
        sendMsgTo udpSocket multicastAddress $ YMDnsRequest host
        timeout resendInterval $ retryM $ do
            (msg, sender) <- recvMsgFrom udpSocket
            case msg of
                YMDnsResponse result -> return $ Just (result, sender)
                _                    -> return Nothing

    let res = case resolveResult of
            NotFound   -> Nothing
            Owner      -> Just sender
            Found addr -> Just addr

    putText $ "resolved: " <> show res
    return res


offloadTask :: Task -> IO (Maybe TaskResponse)
offloadTask t = withSocketsDo $ do
    udpSocket <- createUdpSocket

    putText "sending request"
    sendMsgTo udpSocket multicastAddress $ YMDnsTask t
    (msg, _) <- recvMsgFrom udpSocket
    case msg of
      YMDnsTaskResponse r -> pure r
      e                   -> panic ("Unknown response: " <> show e)
