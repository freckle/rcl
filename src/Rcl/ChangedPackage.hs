module Rcl.ChangedPackage
  ( ChangedPackage(..)
  , getResolverDiff
  , resolverDiffUrl
  )
where

import RIO

import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (unpack)
import Data.These
import Data.Version
import Network.HTTP.Simple
import Rcl.App
import Rcl.PackageName
import Rcl.Resolver

-- | Details for a changed package
data ChangedPackage = ChangedPackage
  { cpName :: PackageName
  , cpDiff :: These Version Version
  -- ^ How did this package change
  --
  -- - @'This'@ means removed, and was this version
  -- - @'That'@ means added, and is that version
  -- - @'These'@ means a change in version
  --
  }

data ResolverDiff = ResolverDiff
  { comparing :: [Text]
  , diff :: HashMap PackageName (HashMap Resolver Version)
  }
  deriving stock Generic
  deriving anyclass FromJSON

getResolverDiff :: Resolver -> Resolver -> RIO App [ChangedPackage]
getResolverDiff fromResolver toResolver = do
  req <- parseRequestThrow $ resolverDiffUrl fromResolver toResolver
  mapMaybe (uncurry changedPackage)
    . HashMap.toList
    . diff
    . getResponseBody
    <$> httpJSON req
 where
  changedPackage name info = do
    change <- maybeToThese
      (HashMap.lookup fromResolver info)
      (HashMap.lookup toResolver info)
    pure ChangedPackage { cpName = name, cpDiff = change }

resolverDiffUrl :: Resolver -> Resolver -> String
resolverDiffUrl fromResolver toResolver =
  "https://www.stackage.org/diff/"
    <> unpack (unResolver fromResolver)
    <> "/"
    <> unpack (unResolver toResolver)

maybeToThese :: Maybe a -> Maybe b -> Maybe (These a b)
maybeToThese (Just a) Nothing = Just $ This a
maybeToThese Nothing (Just b) = Just $ That b
maybeToThese (Just a) (Just b) = Just $ These a b
maybeToThese Nothing Nothing = Nothing
