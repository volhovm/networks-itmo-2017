{-# LANGUAGE TemplateHaskell #-}
-- | Options parsing from CLI

module Options where

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
    deriving (Show)

data Opts = Opts
    { action       :: Action
    , ymdnsTimeout :: Int
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

actionParser :: Parser Action
actionParser = subparser $ serveParser <> requestParser

optsParser :: Parser Opts
optsParser =
    Opts <$>
    actionParser <*>
    option
        auto
        (long "ymdns-timeout" <> metavar "MILLISECONDS" <> value 3 <>
         help "Number of seconds to abort connection after")

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
