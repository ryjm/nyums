--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Codec.Binary.UTF8.String (encode)
import           Control.Arrow (arr, (>>>), (>>^))
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Char (isAlphaNum, isAscii)
import           Data.FileStore (darcsFileStore)
import           Data.FileStore.Utils (runShellCommand)
import           Data.List (nub, sort)
import qualified Data.Map as M (fromList, lookup, Map)
import           Data.Monoid (mempty, mconcat, mappend)
import           Network.HTTP (urlEncode)
import           Network.URI (unEscapeString)
import           System.Directory (removeFile)
import           System.IO.Unsafe (unsafePerformIO)
import           Text.HTML.TagSoup (renderTagsOptions,parseTags,renderOptions, optMinimize, Tag(TagOpen))
import           Text.Printf (printf)

import           Control.Applicative ((<$>))
import           Hakyll
import           Text.Pandoc (bottomUp, HTMLMathMethod(MathML), Inline(Code, Link, Str), Pandoc, WriterOptions(..))

--------------------------------------------------------------------------------
siteurl :: String
siteurl = "http://math.nyu.edu/math_club"

main :: IO ()
main = hakyll $ do
  
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "blog.markdown" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/blog.html" blogCtx
        >>= loadAndApplyTemplate "templates/default.html" blogCtx
        >>= relativizeUrls

  match "*.markdown" $ do
    route   $ setExtension "html"
    compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls


  match "index.html" $ do
    route idRoute
    compile $ do
      let indexCtx = field "posts" $ \_ ->
            postList $ fmap (take 3) . recentFirst
        
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler
  


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  constField "author" "jake" `mappend`
--  constField "description" "N/A" `mappend`
  dateField "created" "%d %b %Y" `mappend`
  dateField "date" "%B %e, %Y" `mappend`
  modificationTimeField "mtime" "%d %b %Y" `mappend`
  defaultContext

-------------------------------------------------------------------------------
blogCtx :: Context String
blogCtx =
  field "posts" (\_ -> postList recentFirst) `mappend`
  modificationTimeField "mtime" "%d %b %Y" `mappend`
  constField "author" "jake" `mappend`
  constField "title" "Blog" `mappend`
  defaultContext

--------------------------------------------------------------------------------
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
  posts   <- sortFilter =<< loadAll "posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  applyTemplateList itemTpl postCtx posts

--------------------------------------------------------------------------------

-- hakyll config with custom pandoc config
pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions {
  -- so we can have a TOC:
  writerHtml5 = True,
  writerStandalone = True,
  writerTableOfContents = True,
  writerTemplate = "<div id=\"TOC\">$toc$</div>\n$body$",
  writerSectionDivs = True }




