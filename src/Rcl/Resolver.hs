module Rcl.Resolver
  ( Resolver(..)
  )
where

import RIO

import Data.Aeson
import Data.Text (Text)

newtype Resolver = Resolver
  { unResolver :: Text
  }
  deriving newtype (Eq, Show, Hashable, FromJSONKey, IsString)
