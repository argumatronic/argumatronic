--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import qualified Data.Set as S
import           Hakyll
import           Hakyll.Core.Configuration
import           Text.Pandoc.Options

--------------------------------------------------------------------------------
--config :: Configuration
--config = defaultConfiguration
--         { deployCommand = "rsync --rsync-path="sudo rsync" -avz -e ssh ./_site/ ubuntu@argumatronic.com:/var/www/argumatronic/" }

config :: Configuration
config = defaultConfiguration
         { deployCommand = "rsync -avz -e 'ssh -i ~/.ssh/freya.pem' ./_site/ ubuntu@argumatronic.com:/var/www/argumatronic/" }

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
     { feedTitle       = "argumatronic"
     , feedDescription = "FP/Haskell blog"
     , feedAuthorName  = "Julie Moronuki"
     , feedAuthorEmail = "srs_haskell_cat@aol.com"
     , feedRoot        = "http://argumatronic.com/"
     }
     
main :: IO ()
main = hakyllWith config $ do
   match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

   match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

   match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

   match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

   match (fromList ["about.md", "contact.markdown", "noobs.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

   match "posts/*" $ do
        tags <- buildTags "posts/*" (fromCapture "tags/*.html") 
        tagsRules tags $ \tag pattern -> do 
          let title = "Posts tagged \"" ++ tag ++ "\"" 
          route idRoute 
          compile $ do 
            posts <- recentFirst =<< loadAll pattern 
            let ctx = constField "title" title 
                           `mappend` listField "posts" postCtx (return posts) 
                           `mappend` defaultContext 
            makeItem "" >>= loadAndApplyTemplate "templates/tags.html" (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

   create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

   create ["rss.xml"] $ do
        route idRoute
        compile $ do
          let feedCtx = postCtx `mappend` bodyField "description"
          posts <- fmap (take 10) . recentFirst =<<
                   loadAllSnapshots "posts/*" "content"
          renderRss feedConfig feedCtx posts


   match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" ""        `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

   match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String 
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx
