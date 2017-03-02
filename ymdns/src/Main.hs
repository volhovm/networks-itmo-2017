module Main where

import           Universum

import           FileSharing (runReportServer)
import           Options     (Action (..), Opts (..), getOptions)
import           Resolving

main :: IO ()
main = do
    o@Opts{..} <- getOptions
    putText $ "Launched with opts: " <> show o
    case action of
        YMServe {..}  -> serveProducer hostname >> runReportServer port filesDir
        YMRequest req -> sendRequest req >> waitForResponse >>= downloadFile
