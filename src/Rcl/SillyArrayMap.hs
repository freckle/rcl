{-# LANGUAGE TypeApplications #-}

-- | Stackage changed its JSON representation for package differences
--
-- It used to be nested objects:
--
-- @
-- { diff:
--   { some-package:    { lts-x: version-x, lts-y: version-y }
--   , another-package: { lts-x: version-x, lts-y: version-y }
--   , ...
--   }
-- }
-- @
--
-- which was very easy to work with in Haskell (map of maps).
--
-- Now it's this:
--
-- @
-- { diff:
--   [ ["some-package",    { lts-x: version-x, lts-y: version-y }]
--   , ["another-package", { lts-x: version-x, lts-y: version-y }]
--   , ...
--   ]
-- }
-- @
--
-- Which is /not/ easy to work with in Haskell (list of heterogenious list).
--
-- I guess is nicer for JavaScript? Anyway, I'm representing this silly type and
-- JSON parser here, to isolate the mess. It allows a drop-in change in my main
-- type to fix the parsing.
--
module Rcl.SillyArrayMap
  ( SillyArrayMap
  , toList
  ) where

import RIO hiding (toList)

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V
import Unsafe.Coerce (unsafeCoerce)

newtype SillyArrayMap k v = SillyArrayMap
  { unSillyArrayMap :: HashMap k v
  }

toList :: SillyArrayMap k v -> [(k, v)]
toList = HashMap.toList . unSillyArrayMap

instance (Eq k, Hashable k, FromJSONKey k, FromJSON v) => FromJSON (SillyArrayMap k v) where
  parseJSON =
    withArray "SillyArrayMap"
      $ fmap (SillyArrayMap . HashMap.fromList)
      . traverse parseSillyArrayElem
      . V.toList

parseSillyArrayElem :: (FromJSONKey k, FromJSON v) => Value -> Parser (k, v)
parseSillyArrayElem = withArray "SillyArrayMapElement" $ go . V.toList
 where
  go = \case
    [k, v] -> (,) <$> parseJSONKey k <*> parseJSON v
    _ -> fail "Silly Array element wasn't exactly two keys"

parseJSONKey :: forall k . FromJSONKey k => Value -> Parser k
parseJSONKey v = case fromJSONKey @k of
  FromJSONKeyCoerce{} -> withText' $ pure . unsafeCoerce
  FromJSONKeyText f -> withText' $ pure . f
  FromJSONKeyTextParser f -> withText' f
  FromJSONKeyValue f -> f v
  where withText' f = withText "Key" f v
