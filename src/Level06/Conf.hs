{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Level06.Conf
    ( parseOptions
    ) where

import           GHC.Word                 (Word16)

import           Data.Bifunctor           (first)
import           Data.Monoid              ((<>))
import           Data.Semigroup           (Last (..))
import           Data.Text                (Text, pack)

import           Waargonaut.Decode.Error   (DecodeError (ParseFailed))

import           Level06.AppM             (AppM (..), liftEither)
import           Level06.Types            (Conf(..), ConfigError(..),
                                           DBFilePath (DBFilePath), PartialConf (..),
                                           Port (Port))

import           Level06.Conf.CommandLine (commandLineParser)
import           Level06.Conf.File        (parseJSONConfigFile)

-- | For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf
  :: PartialConf
defaultConf = PartialConf
              (Just . Last . Port $ 3000)
              (Just . Last . DBFilePath $ "app_db.db")
  --error "defaultConf not implemented"

-- | We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig c = case (pcPort c, pcDBFilePath c) of
  (Nothing, _) -> Left (BadConfFile . ParseFailed . pack $ "pcPort is nothing")
  (_, Nothing) -> Left (BadConfFile . ParseFailed . pack $ "pcDBFilePath is nothing")
  (Just (Last port), Just (Last db)) -> Right (Conf port db)
  -- error "makeConfig not implemented"

-- | This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
--
-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions
  :: FilePath
  -> AppM ConfigError Conf
parseOptions fp = (AppM $ do
  cmdConf <- commandLineParser
  fileConf <- runAppM (parseJSONConfigFile fp)
  return $ (<>) (defaultConf <> cmdConf) <$> fileConf)
  --return $  pure defaultConf <> fileConf <> pure cmdConf )
  >>= liftEither . makeConfig
  -- Parse the options from the config file: "files/appconfig.json"
  -- Parse the options from the commandline using 'commandLineParser'
  -- Combine these with the default configuration 'defaultConf'
  -- Return the final configuration value
  -- error "parseOptions not implemented"
