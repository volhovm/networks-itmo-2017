{-# LANGUAGE TemplateHaskell #-}
-- | Options parsing from CLI

module Options
       ( Action (..)
       , Opts (..)
       , getOptions
       ) where

import           Options.Applicative.Simple (CommandFields, Mod, Parser, auto, command,
                                             help, info, long, metavar, option, progDesc,
                                             short, simpleOptions, simpleVersion,
                                             strOption, subparser, value)
import           Universum

import           Paths_ymdns                (version)

data Action
    = YMServe { port     :: Int
              , filesDir :: FilePath
              , hostname :: String}
    | YMRequest { requestString :: String}
    | YMSendWorker { taskTimeout :: Int }
    | YMCancelWorker { taskTid :: Word32 }
    deriving (Show)

data Opts = Opts
    { action       :: Action
    , ymdnsTimeout :: Int
    , ymdnsK       :: Int
    } deriving (Show)

serveParser :: Mod CommandFields Action
serveParser = command "serve" $ info opts desc
  where
    opts =
        YMServe <$>
        option
            auto
            (long "port" <> short 'p' <> metavar "INTEGER" <>
             help "Port to serve file sharing service on") <*>
        strOption
            (long "filesDir" <> metavar "FILEPATH" <>
             help "Directory to share files from") <*>
        strOption
            (long "hostname" <> metavar "HOSTNAME" <>
             help "Your preferred hostname to use in ymdns")
    desc = progDesc "Serve w/ a hostname as file sharing server"

requestParser :: Mod CommandFields Action
requestParser = command "request" $ info opts desc
  where
    opts =
        YMRequest <$>
        strOption
            (long "request" <> metavar "HOST:PORT/ENDPOINT" <>
             help "Ask for file using ymdns hostname resolving")
    desc =
        progDesc "Make a request to http server resolving hostname using ymdns"

sendWorkerParser :: Mod CommandFields Action
sendWorkerParser = command "sendworker" $ info opts desc
  where
    opts =
        YMSendWorker <$>
        option auto
            (long "delay" <> metavar "INT" <>
             help "Worker task delay")
    desc = progDesc "Offload work to nodes"

cancelWorkerParser :: Mod CommandFields Action
cancelWorkerParser = command "cancelworker" $ info opts desc
  where
    opts =
        YMCancelWorker <$>
        option auto
            (long "tid" <> metavar "INT" <>
             help "Id of task we want to cancel")
    desc = progDesc "Offload work to nodes"


actionParser :: Parser Action
actionParser =
    subparser $
    serveParser <> requestParser <> sendWorkerParser <> cancelWorkerParser

optsParser :: Parser Opts
optsParser =
    Opts <$>
    actionParser <*>
    option
        auto
        (long "ymdns-timeout" <> metavar "MILLISECONDS" <> value 3 <>
         help "Number of seconds to abort connection after") <*>
    option
        auto
        (long "kneighbours" <> metavar "INT" <> value 2 <>
         help "Algorithm constant -- radius of responders")

getOptions :: IO Opts
getOptions = do
    (res, ()) <-
        simpleOptions
            ("ymdns, " <> $(simpleVersion version))
            "YMDNS executable"
            "YMDNS executable"
            optsParser
            empty
    return res
