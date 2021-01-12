module Rcl.Web.Templates
  ( root
  )
where

import RIO

import Data.Version (Version, showVersion)
import Lucid hiding (for_)
import Rcl.PackageName
import Rcl.Resolver
import Rcl.Run (Change(..), Changes(..))
import RIO.Text (pack)
import qualified RIO.Text as T
import qualified Text.Megaparsec as M
import qualified Text.MMark as MMark

root :: [Resolver] -> Changes -> Maybe Text -> Html ()
root resolvers changes@Changes {..} mAlert =
  layout resolvers changes mAlert $ traverse_ (uncurry changeTemplate) cChanges

layout :: [Resolver] -> Changes -> Maybe Text -> Html () -> Html ()
layout resolvers Changes {..} mAlert inner = do
  head_ $ do
    title_ "Resolver Changelog"
    link_
      [ rel_ "stylesheet"
      , type_ "text/css"
      , href_
        "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"
      ]
    style_ css
  body_ $ do
    nav_ [class_ "navbar navbar-dark bg-dark fixed-top"] $ do
      div_ [class_ "container-fluid"] $ do
        a_ [class_ "navbar-brand"] "Resolver Changelog"
        form_ [class_ "d-flex"] $ do
          resolverSelect "from" resolvers cFromResolver
          resolverSelect "to" resolvers cToResolver
          button_ [class_ "btn btn-outline-success", type_ "submit"] "Go"
    main_ [class_ "container"] $ do
      traverse_
        (div_ [class_ "alert alert-info", role_ "alert"] . toHtml)
        mAlert
      inner

resolverSelect :: Text -> [Resolver] -> Maybe Resolver -> Html ()
resolverSelect name resolvers mSelected =
  select_ [name_ name, class_ "form-control me-2"] $ do
    for_ resolvers $ \option -> do
      let
        attrs
          | Just option == mSelected
          = [value_ $ unResolver option, selected_ "true"]
          | otherwise
          = [value_ $ unResolver option]
      option_ attrs $ toHtml $ unResolver option

changeTemplate :: PackageName -> Change -> Html ()
changeTemplate name = \case
  Removed v -> packageSection "removed" name v $ pure ()
  Added v -> packageSection "added" name v $ pure ()
  Changed fromV toV md
    | toV > fromV -> packageSection "upgraded" name toV $ do
      p_ $ toHtml $ "Previous version: " <> pack (showVersion fromV)
      changeLogDetails md
    | otherwise -> packageSection "downgraded" name toV $ do
      p_ $ toHtml $ "Previous version: " <> pack (showVersion fromV)
      changeLogDetails md

packageSection :: Text -> PackageName -> Version -> Html () -> Html ()
packageSection cls name v inner = section_ [class_ cls] $ do
  p_ $ do
    strong_ $ toHtml $ unPackageName name <> "-" <> pack (showVersion v)
    toHtml $ " was " <> cls
  inner

changeLogDetails :: Text -> Html ()
changeLogDetails md = details_ $ do
  summary_ "Changelog"
  case MMark.parse "<input>" md of
    Left bundle -> do
      p_ "Unable to parse Changelog:"
      pre_ $ toHtml $ M.errorBundlePretty bundle
    Right r -> MMark.render r

css :: Text
css = T.unlines
  [ "main {"
  , "  margin-top: 85px;"
  , "}"
  , ""
  , "section {"
  , "  margin: 1em;"
  , "  padding: 1em;"
  , "  border-radius: 0.5em;"
  , "}"
  , ""
  , "section.added {"
  , "  border: 1px solid green;"
  , "}"
  , ""
  , "section.removed {"
  , "  border: 1px solid red;"
  , "}"
  , ""
  , "section.upgraded, section.downgraded {"
  , "  border: 1px solid grey;"
  , "}"
  , ""
  , ".me-2 {"
  , "  margin-right: .5rem!important;"
  , "}"
  ]
