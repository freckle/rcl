module Rcl.Run
  ( fetchChangesMarkdown
  )
where

import RIO

import Data.List (sortOn)
import qualified Data.Set as Set
import Data.Text (pack)
import qualified Data.Text as T
import Data.These
import Data.Version (Version, showVersion)
import Rcl.App
import Rcl.ChangedPackage
import Rcl.Changelog
import Rcl.Options
import Rcl.PackageName

fetchChangesMarkdown :: RIO App [Text]
fetchChangesMarkdown = do
  Options {..} <- asks appOptions

  logDebug
    $ "Fetching change details between "
    <> displayShow oFromResolver
    <> " and "
    <> displayShow oToResolver

  changedPackages <- getResolverDiff oFromResolver oToResolver

  let
    relevantPackages = sortOn cpName
      $ filter ((`Set.member` oDependencies) . cpName) changedPackages

  logDebug
    $ "Resolver differs in "
    <> displayShow (length changedPackages)
    <> " package(s), "
    <> displayShow (length relevantPackages)
    <> " are dependencies"

  for relevantPackages $ \ChangedPackage {..} -> do
    change <- case cpDiff of
      This version -> pure $ Removed version
      That version -> pure $ Added version
      These fromVersion toVersion ->
        Changed fromVersion toVersion
          <$> getChangelogFromTo cpName toVersion fromVersion

    logDebug $ "Changed " <> displayShow cpName <> ": " <> displayShow change
    pure $ renderChanged cpName change

data Change
  = Removed Version
  | Added Version
  | Changed Version Version Text
  deriving Show

renderChanged :: PackageName -> Change -> Text
renderChanged name change = T.unlines $ header <> case change of
  Removed version -> ["Removed (was " <> tshowVersion version <> ")"]
  Added version -> ["Added at version " <> tshowVersion version]
  Changed fromVersion toVersion changelog ->
    [ "Changed from "
      <> tshowVersion fromVersion
      <> " to "
      <> tshowVersion toVersion
    , ""
    , quote changelog
    ]
 where
  header = ["## " <> unPackageName name, ""]
  quote = T.unlines . map ("> " <>) . T.lines
  tshowVersion = pack . showVersion
