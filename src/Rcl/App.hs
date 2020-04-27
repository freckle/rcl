{-# LANGUAGE StrictData #-}

module Rcl.App
  ( App(..)
  , runApp
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

runApp :: Options -> RIO App a -> IO a
runApp options@Options {..} f = do
  logOptions <- logOptionsHandle stderr oDebug
  withLogFunc logOptions $ \logFunc ->
    let app = App { appLogFunc = logFunc, appOptions = options }
    in runRIO app f
