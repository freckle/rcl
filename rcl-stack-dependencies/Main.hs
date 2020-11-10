{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  )
where

import RIO

import Data.Aeson
import Data.Text.IO (putStrLn)
import qualified Data.Yaml as Yaml
import qualified RIO.ByteString.Lazy as BSL
import RIO.FilePath ((</>))
import qualified RIO.HashMap as HashMap
import RIO.List (headMaybe, sort)
import RIO.Process
import qualified RIO.Text as T

newtype Local = Local { path :: FilePath }
  deriving stock Generic
  deriving anyclass FromJSON

class HasDependencies a where
    getDependencies :: a -> [Text]

newtype Target = Target { dependencies :: Maybe [Text] }
  deriving stock Generic
  deriving anyclass FromJSON

instance HasDependencies Target where
  getDependencies Target { dependencies } = fromMaybe [] dependencies

newtype NamedTargets = NamedTargets { targets :: HashMap Text Target }
  deriving newtype FromJSON

instance HasDependencies NamedTargets where
  getDependencies = concatMap getDependencies . HashMap.elems . targets

data Package = Package
  { dependencies :: Maybe [Text]
  , library :: Target
  , executables :: Maybe NamedTargets
  , tests :: Maybe NamedTargets
  }
  deriving stock Generic
  deriving anyclass FromJSON

instance HasDependencies Package where
  getDependencies Package { dependencies, library, executables, tests } =
    concat
      [ fromMaybe [] dependencies
      , getDependencies library
      , maybe [] getDependencies executables
      , maybe [] getDependencies tests
      ]

main :: IO ()
main = do
  dependencies <- runSimpleApp $ do
    query <- readProcessYaml "stack" ["query", "locals"]
    packages <- traverse (Yaml.decodeFileThrow . (</> "package.yaml") . path)
      $ HashMap.elems query
    pure $ filter (`notElem` HashMap.keys query) $ concatMap
      (mapMaybe (headMaybe . T.words) . getDependencies @Package)
      packages

  traverse_ putStrLn $ sort $ nubOrd dependencies

readProcessYaml
  :: ( MonadIO m
     , MonadReader env m
     , HasLogFunc env
     , HasProcessContext env
     , FromJSON a
     )
  => String
  -> [String]
  -> m a
readProcessYaml cmd args = do
  bs <- proc cmd args readProcessStdout_
  either (throwString . show) pure $ Yaml.decodeEither' $ BSL.toStrict bs
