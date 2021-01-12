module Rcl.Run
  ( Changes(..)
  , Change(..)
  , fetchChanges
  )
where

import RIO

import Data.List (sortOn)
import Data.These
import Data.Version (Version)
import Rcl.ChangedPackage
import Rcl.Changelog
import Rcl.PackageName
import Rcl.Resolver

data Changes = Changes
  { cFromResolver :: Maybe Resolver
  , cToResolver :: Maybe Resolver
  , cChanges :: [(PackageName, Change)]
  }

data Change
  = Removed Version
  | Added Version
  | Changed Version Version Text
  deriving Show

fetchChanges
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
  => Resolver
  -> Resolver
  -> m Changes
fetchChanges fromResolver toResolver = do
  logDebug
    $ "fetchChanges "
    <> displayShow fromResolver
    <> " -> "
    <> displayShow toResolver
  changedPackages <- getResolverDiff fromResolver toResolver
  logDebug $ displayShow (length changedPackages) <> " changed package(s)"
  changes <- pooledForConcurrently changedPackages fetchChange

  pure Changes
    { cFromResolver = Just fromResolver
    , cToResolver = Just toResolver
    , cChanges = sortOn fst changes
    }

fetchChange
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
  => ChangedPackage
  -> m (PackageName, Change)
fetchChange ChangedPackage {..} = do
  change <- case cpDiff of
    This version -> pure $ Removed version
    That version -> pure $ Added version
    These fromVersion toVersion ->
      Changed fromVersion toVersion
        <$> getChangelogFromTo cpName toVersion fromVersion
  pure (cpName, change)
