module Main
  ( main
  )
where

import RIO

--import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Rcl.App
--import Rcl.ChangedPackage
import Rcl.Options
import Rcl.Run

main :: IO ()
main = do
  options@Options {..} <- parseOptions

  let
    -- url = resolverDiffUrl oFromResolver oToResolver
      header = [""] -- ["[Stackage diff](" <> pack url <> ")", "", "Changelogs:", ""]

  changes <- runApp options fetchChangesMarkdown
  T.putStrLn $ T.intercalate "\n" $ header <> changes
