{-# LANGUAGE StrictData #-}

module Rcl.App
  ( App(..)
  )
where

import RIO

import Rcl.Options

data App = App
  { appLogFunc :: LogFunc
  , appOptions :: Options
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
