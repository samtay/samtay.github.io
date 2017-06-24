{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>))
import Hakyll
import Text.Pandoc (WriterOptions(..))

main :: IO ()
main = hakyll $ do
  match "img/**" $ do
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
        >>= loadAndApplyTemplate "templates/article.html"    articleCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" articleCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      articles <- recentFirst =<< loadAllSnapshots "articles/*" "content"
      let indexCtx = listField "articles" articleCtx (return articles)
                       <> defaultContext
      getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

customWriterOpts :: Maybe String -> WriterOptions
customWriterOpts (Just "yes")  = writerWithToc
customWriterOpts (Just "true") = writerWithToc
customWriterOpts _             = defaultHakyllWriterOptions

writerWithToc :: WriterOptions
writerWithToc = defaultHakyllWriterOptions
                  { writerTableOfContents = True
                  , writerTemplate        = Just "<div id=\"toc\">$toc$</div>\n$body$"
                  }

articleCtx :: Context String
articleCtx =
  dateField "date" "%B %e, %Y" <> defaultContext

