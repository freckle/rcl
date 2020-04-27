module Rcl.Resolver
  ( Resolver(..)
  )
where

import RIO

import Data.Aeson

newtype Resolver = Resolver
  { unResolver :: Text
  }
  deriving newtype (Eq, Show, Hashable, FromJSONKey, IsString)
