module Rcl.Web.Templates
  ( root
  , failure
  ) where

import RIO

import Data.Version (Version, showVersion)
import Lucid hiding (for_)
import RIO.Text (pack)
import qualified RIO.Text as T
import Rcl.PackageName
import Rcl.Resolver
import Rcl.Run (Change(..), Changes(..))
import Rcl.VersionDiff (VersionDiff(..), versionDiff)
import qualified Text.MMark as MMark
import qualified Text.Megaparsec as M

root :: NonEmpty Resolver -> Changes -> Html ()
root resolvers Changes {..} =
  layout (Just resolvers) (Just cFromResolver) (Just cToResolver) Nothing
    $ traverse_ (uncurry $ changeTemplate cToResolver) cChanges

failure :: Text -> Html ()
failure message = layout Nothing Nothing Nothing (Just message) $ pure ()

layout
  :: Maybe (NonEmpty Resolver)
  -> Maybe Resolver
  -> Maybe Resolver
  -> Maybe Text
  -> Html ()
  -> Html ()
layout mResolvers mFromResolver mToResolver mAlert inner = do
  head_ $ do
    title_ "Resolver Changelog"
    link_
      [ rel_ "stylesheet"
      , type_ "text/css"
      , href_
        "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"
      ]
    style_ css
  body_ [class_ "d-flex flex-column h-100"] $ do
    nav_ [class_ "navbar navbar-dark bg-dark fixed-top"] $ do
      div_ [class_ "container-fluid"] $ do
        a_ [class_ "navbar-brand"] "Resolver Changelog"
        sequenceA_
          $ resolversForm
          <$> mResolvers
          <*> mFromResolver
          <*> mToResolver

    main_ [role_ "main", class_ "flex-shrink-0"] $ do
      div_ [class_ "container"] $ do
        traverse_ alert_ mAlert
        inner

    footer_ [class_ "footer mt-auto py-3"] $ do
      div_ [class_ "container"] $ do
        span_ [class_ "text-muted"] $ do
          "Made with ❤️ by Freckle Engineering. "
          a_ [href_ "https://github.com/freckle/rcl"] "Source"
          "."

alert_ :: Text -> Html ()
alert_ = div_ [class_ "alert alert-info", role_ "alert"] . toHtml

resolversForm :: NonEmpty Resolver -> Resolver -> Resolver -> Html ()
resolversForm resolvers fromResolver toResolver = form_ [class_ "d-flex"] $ do
  resolverSelect "from" resolvers fromResolver
  resolverSelect "to" resolvers toResolver
  button_ [class_ "btn btn-outline-success", type_ "submit"] "Go"

resolverSelect :: Text -> NonEmpty Resolver -> Resolver -> Html ()
resolverSelect name resolvers selected =
  select_ [name_ name, class_ "form-control me-2"] $ do
    for_ resolvers $ \option -> do
      let
        attrs
          | option == selected = [value_ $ unResolver option, selected_ "true"]
          | otherwise = [value_ $ unResolver option]
      option_ attrs $ toHtml $ unResolver option

changeTemplate :: Resolver -> PackageName -> Change -> Html ()
changeTemplate resolver name = \case
  Removed v -> packageSection "removed" resolver name (Nothing, v) $ pure ()
  Added v -> packageSection "added" resolver name (Nothing, v) $ pure ()
  Changed fromV toV md -> do
    let cls = if toV > fromV then "upgraded" else "downgraded"
    packageSection cls resolver name (Just fromV, toV) $ do
      unless (T.null md) $ changeLogDetails md

packageSection
  :: Text
  -> Resolver
  -> PackageName
  -> (Maybe Version, Version)
  -> Html ()
  -> Html ()
packageSection cls resolver name (mFromV, toV) inner =
  section_ [class_ cls] $ do
    p_ $ do
      strong_ $ do
        a_ [href_ packageUrl] $ do
          toHtml $ unPackageName name
      " " <> toHtml cls <> " "
      strong_ $ versionChange mFromV toV
    inner
 where
  packageUrl =
    "https://www.stackage.org/"
      <> unResolver resolver
      <> "/package/"
      <> unPackageName name
      <> "-"
      <> pack (showVersion toV)

versionChange :: Maybe Version -> Version -> Html ()
versionChange mFromV toV = do
  case mFromV of
    Nothing -> span_ $ toHtml $ pack (showVersion toV)
    Just fromV -> do
      let VersionDiff {..} = versionDiff fromV toV
      toHtml commonPrefix
      span_ [class_ "from"] $ toHtml fromSuffix
      " " <> toHtmlRaw @String "&#8594;" <> " "
      toHtml commonPrefix
      span_ [class_ "to"] $ toHtml toSuffix

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
  [ "main > .container {"
  , "  padding-top: 60px;"
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
  , ".footer {"
  , "  background-color: #f5f5f5;"
  , "}"
  , ""
  , ".me-2 {"
  , "  margin-right: .5rem!important;"
  , "}"
  , ".from {"
  , "  background-color: #ffcccc;"
  , "}"
  , ".to {"
  , "  background-color: #d0f0c0;"
  , "}"
  ]
