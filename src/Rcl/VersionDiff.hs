module Rcl.VersionDiff
  ( versionDiff
  , VersionDiff(..)
  ) where

import RIO

import Data.Version (Version, showVersion)

versionDiff :: Version -> Version -> VersionDiff
versionDiff fromV toV = VersionDiff
  { commonPrefix = take prefixLength strFrom
  , fromSuffix = drop prefixLength strFrom
  , toSuffix = drop prefixLength strTo
  }
 where
  strFrom = showVersion fromV
  strTo = showVersion toV
  prefixLength = length $ takeWhile id $ zipWith (==) strFrom strTo

data VersionDiff = VersionDiff
  { commonPrefix :: String
  , fromSuffix :: String
  , toSuffix :: String
  }
  deriving (Eq, Show)
