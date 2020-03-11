{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString             as BS

import           Data.Text                  (Text, pack)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (IOException, try)

import qualified Data.Attoparsec.ByteString as AB

import           Waargonaut                 (Json)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed))

import           Level06.AppM               (AppM (..), liftEither)
import           Level06.Types              (ConfigError (BadConfFile),
                                             PartialConf (PartialConf),
                                             partialConfDecoder)
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> runAppM $ readConfFile "badFileName.no"
-- Left (<YourErrorConstructorHere> "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> runAppM $ readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> AppM ConfigError ByteString
readConfFile fp = AppM $ g (try . BS.readFile $ fp)
  where g :: IO (Either IOException ByteString) -> IO (Either ConfigError ByteString)
        g action = do
          result <- action
          case result of
            Left e ->  pure $ first (BadConfFile . ParseFailed)
              (Left . pack $ show e)
            Right x -> pure $ Right x
  -- Reading a file may throw an exception for any number of
  -- reasons. Use the 'try' function from 'Control.Exception' to catch
  -- the exception and turn it into an error value that is thrown as
  -- part of our 'AppM' transformer.
  --
  -- No exceptions from reading the file should escape this function.
  --
  -- error "readConfFile not implemented"

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile fp =  readConfFile fp >>=
     first f .
     liftEither .
     D.pureDecodeFromByteString AB.parseOnly partialConfDecoder
  where f :: (DecodeError, D.CursorHistory) -> ConfigError
        f (e, _) = BadConfFile . ParseFailed . pack . show $ e
  --error "parseJSONConfigFile not implemented"

-- Go to 'src/Level06/Conf.hs' next.
