module Main
  ( main
  )
where

import RIO

import Rcl.App
import Rcl.Options
import Rcl.Run

main :: IO ()
main = do
  options <- parseOptions
  logOptions <- logOptionsHandle stderr $ oDebug options
  withLogFunc logOptions $ \logFunc ->
    let app = App { appLogFunc = logFunc, appOptions = options }
    in runRIO app run
