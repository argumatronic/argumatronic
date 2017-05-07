--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>), Alternative (..))
import Data.Maybe          (fromMaybe)
import Data.Monoid         ((<>))
import Data.List           (isPrefixOf, isSuffixOf, sortBy, intercalate)
import Data.Time.Format    (parseTimeM, defaultTimeLocale)
import Data.Time.Clock     (UTCTime)
import System.FilePath     (takeFileName)

import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html ((!), toHtml, toValue)
import Text.Blaze.Internal (preEscapedString)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Pandoc.Options ( WriterOptions
                           , writerHTMLMathMethod
                           , HTMLMathMethod(MathJax)
                           , writerHtml5
                           )
import Hakyll
--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
         { deployCommand = "rsync -avz -e 'ssh -i ~/.ssh/freya.pem' ./_site/ ubuntu@argumatronic.com:/var/www/argumatronic/" }
-- deployCommand = "./bin/deploy.sh" -- this would be better ?

-- configuration for rss feed
feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
     { feedTitle       = "argumatronic"
     , feedDescription = "FP/Haskell blog"
     , feedAuthorName  = "Julie Moronuki"
     , feedAuthorEmail = "srs_haskell_cat@aol.com"
     , feedRoot        = "http://argumatronic.com/"
     }

--------------------------------------------------------------------------------
-- name some of the Patterns
staticContent :: Pattern
staticContent = "favicon.ico"
           .||. "images/*"
           .||. "fonts/*"

postsGlob :: Pattern
postsGlob = "posts/*"

-- | "Lift" a compiler into an idRoute compiler.
-- idR :: ... => Compiler (Item String) -> Rules ()
-- https://github.com/zeckalpha/kyle.marek-spartz.org/blob/master/site.hs
idR compiler = do
    route idRoute
    compile compiler


main :: IO ()
main = hakyllWith config $ do
  -- copyFileCompiler and compressCssCompiler
  -- are from Hakyll library
    match staticContent $ idR copyFileCompiler

    match "css/*" $ idR compressCssCompiler
  --for indexCompiler, see below
    match "index.html" $ idR $ indexCompiler
  -- templateCompiler also comes from Hakyll
    match "templates/*" $ compile templateCompiler
  -- i should really pull this out into a separate compiler
  -- function as i did the others, but for now this works
  -- these files are separate pages but are not in /posts
  -- and i don't want the post template applied
    match (fromList ["about.md", "noobs.markdown", "cats.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

  -- this captures the tags from the tags field of each post
  -- as an argument 'tags' that can be passed to the rulesForTags
  -- which makes the Posts tagged page (see below)
    tags <- buildTags postsGlob (fromCapture "tags/*.html")

    rulesForTags tags (\tag -> "Posts tagged \"" ++ tag ++ "\"")

  -- for postCompiler, see below
    match postsGlob $ do
        route $ setExtension "html"
        compile postCompiler

  -- again, the archive compiler could be pulled out of here
  -- as i did with other compilers, but it has its own template
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
  -- this makes the rss feed work ~somehow~
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
          let feedCtx = postCtx <> bodyField "description"
          posts <- fmap (take 10) . recentFirst =<<
                   loadAllSnapshots "posts/*" "content"
          renderRss feedConfig feedCtx posts


-------------------------------------------------------------------------------

-- compiler for posts
-- captures tags from tag field
-- uses the pandocCompilerWith instead of regular pandocCompiler to allow you
-- to add options. writerOptions is defined below, as is the postCtxWithTags
postCompiler :: Compiler (Item String)
postCompiler = do
   tags <- buildTags postsGlob (fromCapture "tags/*.html")
   pandocCompilerWith defaultHakyllReaderOptions writerOptions
     >>= saveSnapshot "content"
     >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
     >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
     >>= relativizeUrls

-- the index is my home page and has its own template
-- i only wanted the list on the home page to have 10 posts
-- for now the postCtx/indexCtx only lists the title and date
-- of each post (not a teaser or the tags)
indexCompiler :: Compiler (Item String)
indexCompiler = do
   posts <- recentFirst =<< loadAll "posts/*"
   let indexCtx =
           listField "posts" postCtx (return posts)
           <> constField "title" ""
           <> defaultContext
   getResourceBody
       >>= applyAsTemplate indexCtx
       >>= loadAndApplyTemplate "templates/default.html" indexCtx
       >>= relativizeUrls

-- these rules create a "page" for each tag
-- uses the postCtx so only lists the title/date of each post
-- you do need a tags html template for this to work
rulesForTags :: Tags -> (String -> String) -> Rules ()
rulesForTags tags titleForTag =
    tagsRules tags $ \tag pattern -> do
    let title = titleForTag tag -- "Posts tagged \"" ++ tag ++ "\""
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title
                  <> listField "posts" postCtx (return posts)
                  <> defaultContext
        makeItem ""
            >>= loadAndApplyTemplate "templates/tags.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

-- Render a simple tag list in HTML, with the tag count next to the item
-- https://github.com/rgoulter/my-hakyll-blog/blob/master/site.hs
-- that's what is supposed to happen but for now i am not using this
renderTagListLines :: Tags -> Compiler (String)
renderTagListLines =
    renderTags makeLink (intercalate ",<br>")
  where
    makeLink tag url count _ _ = renderHtml $
        H.a ! A.href (toValue url) $ toHtml (tag ++ " (" ++ show count ++ ")")

--------------------------------------------------------------------------------
-- Next and previous posts:
-- https://github.com/rgoulter/my-hakyll-blog/commit/a4dd0513553a77f3b819a392078e59f461d884f9
-- these are necessary to make the next and prev buttons work
-- then you also have to add a prev-next html template and then
-- add that template to your post.html template
prevPostUrl :: Item String -> Compiler String
prevPostUrl post = do
  posts <- getMatches postsGlob
  let ident = itemIdentifier post
      sortedPosts = sortIdentifiersByDate posts
      ident' = itemBefore sortedPosts ident
  case ident' of
    Just i -> (fmap (maybe empty toUrl) . getRoute) i
    Nothing -> empty


nextPostUrl :: Item String -> Compiler String
nextPostUrl post = do
  posts <- getMatches postsGlob
  let ident = itemIdentifier post
      sortedPosts = sortIdentifiersByDate posts
      ident' = itemAfter sortedPosts ident
  case ident' of
    Just i -> (fmap (maybe empty toUrl) . getRoute) i
    Nothing -> empty


itemAfter :: Eq a => [a] -> a -> Maybe a
itemAfter xs x =
  lookup x $ zip xs (tail xs)


itemBefore :: Eq a => [a] -> a -> Maybe a
itemBefore xs x =
  lookup x $ zip (tail xs) xs


urlOfPost :: Item String -> Compiler String
urlOfPost =
  fmap (maybe empty toUrl) . getRoute . itemIdentifier

sortIdentifiersByDate :: [Identifier] -> [Identifier]
sortIdentifiersByDate =
    sortBy byDate
  where
    byDate id1 id2 =
      let fn1 = takeFileName $ toFilePath id1
          fn2 = takeFileName $ toFilePath id2
          parseTime' fn = parseTimeM True defaultTimeLocale "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn
      in compare (parseTime' fn1 :: Maybe UTCTime) (parseTime' fn2 :: Maybe UTCTime)

-- --------------------------------------------------------------------------------

-- writerOptions for the postCompiler
-- adds MathJax so i could theoretically write fancy math in my posts
writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js"
    , writerHtml5          = True
    }

-- i'm not really sure this is the ideal way to set up these contexts
-- but it does work. may reconsider later, esp if i add teasers.
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    field "nextPost" nextPostUrl <>
    field "prevPost" prevPostUrl <>
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

-- postsCtx :: [Item String] -> Context String
-- postsCtx posts =
--     listField "posts" postCtx (return posts) <>
--     constField "description" "Writings"      <>
--     constField "title" "Blog"                <>
--     defaultContext
