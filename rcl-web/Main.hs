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
      let (defaultFrom, defaultTo) = lastTwo resolvers
      changes <- fetchChanges
        (fromMaybe defaultFrom mFromResolver)
        (fromMaybe defaultTo mToResolver)
      pure $ Templates.root resolvers changes

-- Get the last two items from a list
--
-- n.b. If a singleton list then the singleton value is returned twice
--
lastTwo :: NonEmpty a -> (a, a)
lastTwo xs = NE.head $ NE.zip (fromMaybe xs $ NE.nonEmpty $ NE.drop 1 xs) xs

paramMay :: Parsable a => LT.Text -> ActionM (Maybe a)
paramMay name = (Just <$> param name) `rescue` (\_ -> pure Nothing)
