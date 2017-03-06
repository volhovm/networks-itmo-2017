{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.URI  (URI (..), URIAuth (..), parseURI)
import           Network.Wreq (get, responseBody)
import           Universum

import           FileSharing  (runReportServer)
import           Options      (Action (..), Opts (..), getOptions)
import           Resolving

main :: IO ()
main = do
    o@Opts{..} <- getOptions
    putText $ "Launched with opts: " <> show o
    case action of
        YMServe {..}  -> do
            serveProducer hostname ymdnsK
            runReportServer port filesDir
        YMRequest{..} -> do
            let uri = fromMaybe (panic "Couldn't parse URI") $
                          parseURI requestString
                host = uriRegName $ fromMaybe (panic "Couldn't get host") $
                                    uriAuthority uri

            host' <- resolveHost (Hostname host) >>= \case
                Nothing -> fail "host not found"
                Just (InetAddress addr _port) -> return $ showHostAddress addr

            let uri' = uri { uriAuthority = (\a -> a { uriRegName = host' }) <$> uriAuthority uri }
                requestAddress = show uri'
            resp <- get requestAddress
            putStrLn $ resp ^. responseBody
        YMSendWorker delay ->
            print =<< offloadTask (Task delay)
