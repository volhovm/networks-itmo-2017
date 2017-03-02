{-# LANGUAGE ScopedTypeVariables #-}
-- | File sharing server/client code.

module FileSharing where

import           Control.Concurrent                   (forkIO)
import           Control.Concurrent.STM               (TVar, newTVarIO, readTVar,
                                                       writeTVar)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL
import           Network.HTTP.Types.Status            (status200, status404)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           System.Directory                     (createDirectoryIfMissing,
                                                       listDirectory)
import           System.FilePath                      (takeFileName)
import           Universum
import           Web.Scotty                           (ScottyM, file, get, notFound,
                                                       param, post, regex, scottyApp,
                                                       status, text)

----------------------------------------------------------------------------
-- Server part
----------------------------------------------------------------------------

fileRetrievalWorker :: FilePath -> TVar [FilePath] -> IO ()
fileRetrievalWorker fp var = action `catch` handler
  where
    action = forever $ do
        createDirectoryIfMissing True fp
        atomically . writeTVar var =<< listDirectory fp
    handler (e :: SomeException) = do
        putText $ "Got error in fileRetrievalWorker: " <> show e
        action `catch` handler

runReportServer :: Int -> FilePath -> IO ()
runReportServer port startDir = do
    var <- newTVarIO []
    void $ forkIO $ fileRetrievalWorker startDir var
    application <- scottyApp $ fileSharingApp var
    Warp.run port $ logStdoutDev  application

fileSharingApp :: TVar [FilePath] -> ScottyM ()
fileSharingApp var = do
    post "/list" $ do
        available <- atomically $ readTVar var
        text $ TL.fromStrict $
            T.intercalate "\n" $ map (T.pack . takeFileName) available
    get (regex "^/download/(.*)$") $ do
        (filename :: FilePath) <- param "1"
        exists <- atomically $ do
            allPaths <- readTVar var
            pure $ find ((filename ==) . takeFileName) allPaths
        maybe (status status404) (\x -> file x >> status status200) exists
    notFound err404
  where
    err404 = status status404 >> text "Not found"
