module Main
  ( main
  )
where

import RIO

import Data.Aeson hiding (json)
import qualified Data.Set as Set
import qualified Prelude as Unsafe
import Rcl.App
import Rcl.ChangedPackage
import Rcl.Options
import Rcl.PackageName
import Rcl.Resolver
import Rcl.Run
import System.Environment (lookupEnv)
import Web.Scotty hiding (Options(..), options)

main :: IO ()
main = do
  port <- maybe 3000 Unsafe.read <$> lookupEnv "PORT"
  debug <- maybe False (not . null) <$> lookupEnv "DEBUG"

  scotty port $ do
    get "/" $ do
      options <-
        Options debug
        <$> (Resolver <$> param "from")
        <*> (Resolver <$> param "to")
        <*> (Set.fromList . map PackageName <$> param "dependencies")

      let url = resolverDiffUrl (oFromResolver options) (oToResolver options)

      changes <- liftIO $ runApp options fetchChangesMarkdown
      json $ object ["diffUrl" .= url, "changesMarkdown" .= changes]
