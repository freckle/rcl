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
      mFromResolver <- paramMay "from"
      mToResolver <- paramMay "to"

      (resolvers, changes, mAlert) <- liftIO $ runSimpleApp $ do
        result@(resolvers, changes, mAlert) <- run mFromResolver mToResolver
        result <$ logInfo
          ("Rendering, "
          <> displayShow (length resolvers)
          <> " Resolver(s), "
          <> displayShow (length $ cChanges changes)
          <> " Changes, "
          <> maybe mempty ((", Alert: " <>) . display) mAlert
          )

      html $ L.renderText $ Templates.root resolvers changes mAlert

run
  :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
  => Maybe Resolver
  -> Maybe Resolver
  -> m ([Resolver], Changes, Maybe Text)
run mFromResolver mToResolver = do
  mResolvers <- NE.nonEmpty <$> fetchResolvers

  case (mResolvers, mFromResolver, mToResolver) of
    -- Unable to fetch the list for selection or inference, but we can make use
    -- of the explicit params to still render changes
    (Nothing, Just fromResolver, Just toResolver) -> do
      changes <- fetchChanges fromResolver toResolver
      pure ([], changes, Just "Error fetching LTS Resolvers")

    -- Can't do anything
    (Nothing, _, _) ->
      pure ([], toChanges [], Just "Error fetching LTS Resolvers")

    -- Can always render, defaulting to first/last for missing inputs
    (Just resolvers, _, _) -> do
      changes <- fetchChanges
        (fromMaybe (NE.last resolvers) mFromResolver)
        (fromMaybe (NE.head resolvers) mToResolver)
      pure (NE.toList resolvers, changes, Nothing)
 where
  toChanges changes = Changes
    { cFromResolver = mFromResolver
    , cToResolver = mToResolver
    , cChanges = changes
    }
paramMay :: Parsable a => LT.Text -> ActionM (Maybe a)
paramMay name = (Just <$> param name) `rescue` (\_ -> pure Nothing)
