module Main where

import           Universum

import           Options   (getOptions)


main :: IO ()
main = do
    o <- getOptions
    putText $ "Launched with opts: " <> show o
