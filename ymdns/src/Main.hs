module Main where

import           Data.Store  (Store (..), encode)
import           Universum

import           FileSharing
import           Options
import           Resolving

main :: IO ()
main = do
    o <- getOptions
    putText $ "Launched with opts: " <> show o
    case action o of
        YMRequest req -> sendRequest req >> waitForResponse >>= downloadFile
        YMServe {..}  -> serveProducer hostname >> runReportServer port filesDir
