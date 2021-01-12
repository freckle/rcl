module Rcl.PackageName
  ( PackageName(..)
  )
where

import RIO

import Data.Aeson

newtype PackageName = PackageName
  { unPackageName :: Text
  }
  deriving newtype (Eq, Ord, Show, Display, Hashable, FromJSONKey, IsString)
