module Rcl.Resolver
  ( Resolver
  , parseResolver
  , unResolver
  , resolverIsLTS
  , fetchResolvers
  )
where

import RIO

import Control.Error.Util (note)
import Data.Aeson
import Data.Hashable.Time ()
import Data.Version (Version, makeVersion, showVersion)
import Network.HTTP.Simple
import RIO.List (sortOn)
import RIO.Text (pack, unpack)
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import qualified RIO.Text.Partial as T (splitOn)
import RIO.Time
import Web.Scotty (Parsable(..))

data Resolver
  = Nightly UTCTime
  | LTS Version
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass Hashable

instance IsString Resolver where
  fromString = either (error . unpack) id . parseResolver . pack

instance FromJSON Resolver where
  parseJSON = withText "Resolver" $ either (fail . unpack) pure . parseResolver

instance FromJSONKey Resolver where
  fromJSONKey =
    FromJSONKeyTextParser $ either (fail . unpack) pure . parseResolver

instance Parsable Resolver where
  parseParam = first TL.fromStrict . parseResolver . TL.toStrict

parseResolver :: Text -> Either Text Resolver
parseResolver name = note invalid $ nightly <|> lts
 where
  invalid = "Unable to parse Resolver: " <> name

  nightly = do
    date <- unpack <$> T.stripPrefix "nightly-" name
    Nightly <$> parseTimeM True defaultTimeLocale nightlyTimeFormat date

  lts = do
    suffix <- T.stripPrefix "lts-" name
    branch <- traverse (readMaybe . unpack) $ T.splitOn "." suffix
    pure $ LTS $ makeVersion branch

unResolver :: Resolver -> Text
unResolver = pack . \case
  Nightly t -> "nightly-" <> formatTime defaultTimeLocale nightlyTimeFormat t
  LTS v -> "lts-" <> showVersion v

resolverIsLTS :: Resolver -> Bool
resolverIsLTS = \case
  Nightly{} -> False
  LTS{} -> True

newtype Snapshot = Snapshot
  { _unSnapshot :: [Text]
  }
  deriving newtype FromJSON

snapshotResolver :: Snapshot -> Either Text Resolver
snapshotResolver = \case
  Snapshot [name, _, _] -> parseResolver name
  Snapshot parts ->
    Left $ "Snapshot Array didn't have parsable parts: " <> pack (show parts)

newtype Snapshots = Snapshots
  { snapshots :: [[Snapshot]]
  }
  deriving stock Generic
  deriving anyclass FromJSON

fetchResolvers :: (MonadIO m, MonadReader env m, HasLogFunc env) => m [Resolver]
fetchResolvers = do
  logDebug "Fetching all Resolvers"
  resolvers <- go [] 1
  logDebug $ "Resolvers: " <> displayShow resolvers
  pure $ sortOn Down $ filter resolverIsLTS resolvers
 where
  go acc page = do
    resolvers <- fetchResolversPage page
    if null resolvers then pure acc else go (acc <> resolvers) (page + 1)

fetchResolversPage
  :: (MonadIO m, MonadReader env m, HasLogFunc env) => Int -> m [Resolver]
fetchResolversPage page = do
  let
    req =
      parseRequest_
        $ "https://www.stackage.org/snapshots?_accept=application/json&page="
        <> show page
  logDebug $ "Fetching /snapshots?page=" <> displayShow page
  resp <- httpJSON req

  let
    (errors, resolvers) =
      partitionEithers
        $ map snapshotResolver
        $ concat
        $ snapshots
        $ getResponseBody resp

  unless (null errors)
    $ logError
    $ "Errors parsing Resolvers: "
    <> displayShow errors

  pure resolvers

nightlyTimeFormat :: String
nightlyTimeFormat = "%Y-%0m-%0d"
