module Main
  ( main
  )
where

import RIO

import qualified Lucid as L
import qualified Prelude as Unsafe
import Rcl.Resolver
import Rcl.Run
import qualified Rcl.Web.Templates as Templates
import qualified RIO.NonEmpty as NE
import qualified RIO.Text.Lazy as LT
import System.Environment (lookupEnv)
import Web.Scotty hiding (Options(..), options)

main :: IO ()
main = do
  port <- maybe 3000 Unsafe.read <$> lookupEnv "PORT"

  scotty port $ do
    get "/" $ do
      ps <- (,) <$> paramMay "from" <*> paramMay "to"
      template <- liftIO $ runSimpleApp $ uncurry run ps
      html $ L.renderText template

    get "/health-check" $ text "OK"

run
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
  => Maybe Resolver
  -> Maybe Resolver
  -> m (L.Html ())
run mFromResolver mToResolver = do
  mResolvers <- NE.nonEmpty <$> fetchResolvers

  case mResolvers of
    Nothing -> pure $ Templates.failure "Unable to fetch all Resolvers"
    Just resolvers -> do
      changes <- fetchChanges
        (fromMaybe (NE.last resolvers) mFromResolver)
        (fromMaybe (NE.head resolvers) mToResolver)
      pure $ Templates.root resolvers changes

paramMay :: Parsable a => LT.Text -> ActionM (Maybe a)
paramMay name = (Just <$> param name) `rescue` (\_ -> pure Nothing)
