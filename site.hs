{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>))
import Hakyll
import Text.Pandoc (WriterOptions(..))

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      customWriterOpts <$> (getUnderlying >>= (`getMetadataField` "toc"))
        >>= pandocCompilerWith defaultHakyllReaderOptions
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
      let indexCtx = listField "posts" postCtx (return posts)
                       <> defaultContext
      getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

customWriterOpts (Just "yes")  = writerWithToc
customWriterOpts (Just "true") = writerWithToc
customWriterOpts _             = defaultHakyllWriterOptions

writerWithToc = defaultHakyllWriterOptions
                  { writerTableOfContents = True
                  , writerTemplate        = Just "<div id=\"toc\">$toc$</div>\n$body$"
                  }

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <> defaultContext

