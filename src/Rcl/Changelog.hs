module Rcl.Changelog
  ( getChangelogFromTo
  ) where

import RIO

import Data.Text (pack, unpack)
import qualified Data.Text as T
import Data.Version
import Network.HTTP.Client (HttpExceptionContent(..))
import Network.HTTP.Simple
import Network.HTTP.Types.Status (status404)
import Rcl.PackageName

getChangelogFromTo
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
  => PackageName
  -> Version
  -> Version
  -> m Text
getChangelogFromTo name version oldVersion = handleJust notFound pure $ do
  let
    url = changelogUrl name version
    req = setRequestHeaders [("Accept", "text/plain")] $ parseRequest_ url
  logDebug
    $ "Fetching changelog for "
    <> display name
    <> " from "
    <> fromString url
  changelogFromTo version oldVersion . getResponseBody <$> httpBS req

notFound :: HttpException -> Maybe Text
notFound = \case
  HttpExceptionRequest _ (StatusCodeException resp _)
    | getResponseStatus resp == status404 -> Just "Changelog not found"
  _ -> Nothing

changelogUrl :: PackageName -> Version -> String
changelogUrl "base" _ =
  "https://raw.githubusercontent.com/ghc/ghc/master/libraries/base/changelog.md"
changelogUrl name version =
  "http://hackage.haskell.org/package/"
    <> unpack (unPackageName name)
    <> "-"
    <> showVersion version
    <> "/changelog"

changelogFromTo :: Version -> Version -> ByteString -> Text
changelogFromTo version oldVersion =
  T.strip
    . T.unlines
    . takeWhile (not . marksVersion oldVersion)
    . dropWhile (not . marksVersion version)
    . T.lines
    . T.replace "\r\n" "\n"
    . decodeUtf8With lenientDecode

marksVersion :: Version -> Text -> Bool
marksVersion version ln = or
  [ tversion `T.isSuffixOf` ln
  , (tversion <> " ") `T.isInfixOf` ln
  , (tversion <> ",") `T.isInfixOf` ln
  , ("[" <> tversion <> "]") `T.isInfixOf` ln
  ]
  where tversion = pack $ showVersion version
