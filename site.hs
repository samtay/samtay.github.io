{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>))
import Hakyll
import Text.Pandoc (WriterOptions(..))

type RenderingFunction = FeedConfiguration
           -> Context String
           -> [Item String]
           -> Compiler (Item String)

main :: IO ()
main = hakyll $ do
  match "img/**" $ do
    route idRoute
    compile copyFileCompiler

  match "fonts/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "articles/*" $ do
    route $ setExtension "html"
    compile $
      customWriterOpts <$> (getUnderlying >>= (`getMetadataField` "toc"))
        >>= pandocCompilerWith defaultHakyllReaderOptions
        >>= loadAndApplyTemplate "layouts/article.html" articleCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "layouts/default.html" articleCtx
        >>= relativizeUrls

  match "pages/*" $ do
    route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "layouts/page.html" siteCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "layouts/default.html" siteCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      articles <- recentFirst =<< loadAllSnapshots "articles/*" "content"
      let indexCtx = listField "articles" articleCtx (return articles)
                       <> siteCtx
      getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "layouts/default.html" indexCtx
          >>= relativizeUrls

  match "layouts/*" $ compile templateCompiler
  match "partials/*" $ compile templateCompiler

  createFeed "atom.xml" renderAtom

  createFeed "feed.xml" renderRss


customWriterOpts :: Maybe String -> WriterOptions
customWriterOpts (Just "yes")  = writerWithToc
customWriterOpts (Just "true") = writerWithToc
customWriterOpts _             = defaultHakyllWriterOptions

writerWithToc :: WriterOptions
writerWithToc = defaultHakyllWriterOptions
                  { writerTableOfContents = True
                  , writerTemplate        = Just "<div id=\"toc\">$toc$</div>\n$body$"
                  }

-- TODO - Load this via config.json ??
siteCtx :: Context String
siteCtx = mconcat
  [ constField "baseurl" "samtay.github.io"
  , constField "protocol" "https"
  , defaultContext
  ]

articleCtx :: Context String
articleCtx =
  dateField "date" "%B %e, %Y" <> siteCtx

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = "A heaping teaspoon of Haskell"
  , feedDescription = "Articles from an aspiring functional programmer"
  , feedAuthorName  = "Sam Tay"
  , feedAuthorEmail = "sam.chong.tay@gmail.com"
  , feedRoot        = "https://samtay.github.io/"
  }

createFeed :: Identifier -> RenderingFunction -> Rules ()
createFeed name renderFunc = create [name] $ do
  route idRoute
  compile $ do
    let feedCtx = articleCtx <> bodyField "description"
    articles <- fmap (take 10) . recentFirst
      =<< loadAllSnapshots "articles/*" "content"
    renderFunc feedConfig feedCtx articles
