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
                                                       doesFileExist, listDirectory)
import           System.FilePath                      (takeFileName)
import           Universum
import           Web.Scotty                           (ScottyM, file, get, notFound,
                                                       param, post, regex, scottyApp,
                                                       status, text)

----------------------------------------------------------------------------
-- Server part
----------------------------------------------------------------------------

retrieveFiles :: FilePath -> IO [FilePath]
retrieveFiles dir = listDirectory dir >>= filterM doesFileExist

runReportServer :: Int -> FilePath -> IO ()
runReportServer port startDir = do
    createDirectoryIfMissing True startDir
    application <- scottyApp $ fileSharingApp startDir
    Warp.run port $ logStdoutDev  application

fileSharingApp :: FilePath -> ScottyM ()
fileSharingApp dir = do
    post "/list" $ do
        available <- liftIO $ retrieveFiles dir
        text $ TL.fromStrict $
            T.intercalate "\n" $ map (T.pack . takeFileName) available
    get (regex "^/download/(.*)$") $ do
        (filename :: FilePath) <- param "1"
        exists <- liftIO $ do
            allPaths <- retrieveFiles dir
            pure $ find ((filename ==) . takeFileName) allPaths
        maybe (status status404) (\x -> file x >> status status200) exists
    notFound err404
  where
    err404 = status status404 >> text "Not found"
