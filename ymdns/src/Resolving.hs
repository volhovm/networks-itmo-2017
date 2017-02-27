{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | YMDNS protocol implementation, all those multicast-related
-- things.

module Resolving where

import           Data.Store (Size (..), Store (..))
import           Universum  hiding (ByteString)

----------------------------------------------------------------------------
-- Network message types
----------------------------------------------------------------------------

-- | Hostname we can reserve. Should be <256 chars, ascii low-letters only.
newtype Hostname = Hostname
    { getHostname :: String
    } deriving (Show)

-- | Resolve map from hostname to ip.
newtype ResolveMap = ResolveMap
    { getResolveMap :: [(Hostname, Int32)]
    } deriving (Show)

-- | YMDns message type
data YMDnsMsg
    = YMDnsJoin { ymdJoinHostname :: Hostname}
    | YMDnsShare { ymdShared :: ResolveMap }
    | YMDnsRequest { ymdReq :: Hostname}
    | YMDnsResponse { ymdResp :: ResolveMap }
    | YMDnsHeartbeat

----------------------------------------------------------------------------
-- Binary serialization
----------------------------------------------------------------------------

checkHostnameChar :: (MonadFail m) => Word8 -> m ()
checkHostnameChar c =
    unless ((c >= 97 && c <= 122) || c == 46) $
        fail $ "Hostname character not in range [(a..z)|.]: '" <>
               [chr $ fromIntegral c] <> "'"

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
