module Rcl.Changelog
  ( getChangelogFromTo
  , changeLogLinesTo
  )
where

import RIO

import Data.Text (pack, unpack)
import qualified Data.Text as T
import Data.Version
import Network.HTTP.Simple
import Rcl.App
import Rcl.PackageName

getChangelogFromTo :: PackageName -> Version -> Version -> RIO App Text
getChangelogFromTo name toVersion fromVersion = do
  req <-
    parseRequestThrow
    $ "http://hackage.haskell.org/package/"
    <> unpack (unPackageName name)
    <> "-"
    <> showVersion toVersion
    <> "/changelog"

  T.strip
    . T.unlines
    . drop 1 -- Hackage's automatic title
    . changeLogLinesTo fromVersion
    . getResponseBody
    <$> httpBS req

changeLogLinesTo :: Version -> ByteString -> [Text]
changeLogLinesTo version =
  takeWhile (not . marksVersion) . T.lines . decodeUtf8With lenientDecode
 where
  -- Yes, this will need futzing as we discover odd changelog formats, but it's
  -- not too bad so far.
  marksVersion ln =
    tversion
      `T.isSuffixOf` ln
      || (tversion <> " ")
      `T.isInfixOf` ln
      || (tversion <> ",")
      `T.isInfixOf` ln

  tversion = pack $ showVersion version
