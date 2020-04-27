{-# LANGUAGE StrictData #-}

module Rcl.Options
  ( Options(..)
  , parseOptions
  )
where

import RIO

import qualified Data.Set as Set
import Options.Applicative
import Rcl.PackageName
import Rcl.Resolver

data Options = Options
  { oDebug :: Bool
  , oFromResolver :: Resolver
  , oToResolver :: Resolver
  , oDependencies :: Set PackageName
  }

parseOptions :: IO Options
parseOptions = execParser $ info (parser <**> helper) $ fullDesc <> progDesc
  "Stackage resolver changelog"

-- brittany-disable-next-binding

parser :: Parser Options
parser = Options
  <$> switch (short 'd' <> long "debug")
  <*> strOption (short 'f' <> long "from" <> metavar "RESOLVER")
  <*> strOption (short 't' <> long "to" <> metavar "RESOLVER")
  <*> (Set.fromList <$> many (strArgument (metavar "PACKAGE")))
