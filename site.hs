--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import Hakyll
    ( getResourceBody,
      makeItem,
      saveSnapshot,
      loadAll,
      loadAllSnapshots,
      defaultConfiguration,
      copyFileCompiler,
      idRoute,
      setExtension,
      compile,
      create,
      match,
      route,
      hakyllWith,
      compressCssCompiler,
      renderRss,
      relativizeUrls,
      bodyField,
      constField,
      dateField,
      defaultContext,
      listField,
      applyAsTemplate,
      loadAndApplyTemplate,
      templateBodyCompiler,
      recentFirst,
      pandocCompilerWith,
      defaultHakyllReaderOptions,
      defaultHakyllWriterOptions,
      Configuration(destinationDirectory),
      FeedConfiguration(..),
      Context )

import Text.Pandoc.Options
--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros ]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = defaultExtensions `mappend` pandocExtensions `mappend` extensionsFromList mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

main :: IO ()
main = hakyllWith config $ do
    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- match (fromList ["about.rst", "contact.markdown"]) $ do
    --     route   $ setExtension "html"
    --     compile $ pandocCompiler
    --         >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --         >>= relativizeUrls


    match "posts/*.markdown" $ do
        route $ setExtension "html"
        --compile $ pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions
        --        $ renderFormulae defaultPandocForumlaOptions
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
    
    -- The rss feed of the posts
    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- 
          fmap (Prelude.take 10) . recentFirst
            =<< loadAllSnapshots "posts/*" "content"

        renderRss feedConfiguration feedCtx posts
    -- match "404.html" $ do
    --     route idRoute
    --     compile $ pandocCompiler
    --         >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --         >>= relativizeUrls

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
    


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["contact.html"] $ do
            route idRoute
            compile $ do
                let contactCtx =
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/contact.html" contactCtx
                    >>= loadAndApplyTemplate "templates/default.html" contactCtx
                    >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "Todos os Posts",
      feedDescription = "Todos os Posts da página IFF da Depressão",
      feedAuthorName = "Paulo Villaça",
      feedAuthorEmail = "pauloarturvillaca@gmail.com",
      feedRoot = "https://algumcorrupto.github.io/IFF-depressao/"
    }