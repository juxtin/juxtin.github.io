--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Char    (isDigit, chr)
import           Data.Maybe   (fromMaybe)
import           Data.Monoid  (mappend)
import           Hakyll
import qualified Text.RegexPR as RE

circledNumberCode :: Integer -> Maybe Char
circledNumberCode x =
  if 0 < x && x <= 20
    then Just $ unicodify x
    else Nothing
  where
    unicodify :: Integer -> Char
    unicodify x =
      let idx = fromInteger (x + 9311) in
        chr idx

substituteCircleNums :: String -> String -> String
substituteCircleNums re = RE.gsubRegexPRBy re tryToCircle
  where
    tryToCircle :: String -> String
    tryToCircle s =
      let
        digits = filter isDigit s
        i = read digits :: Integer
      in
        case circledNumberCode i of
          Just c -> [c]
          Nothing -> "(" ++ digits ++ ")"

substituteCircleNumsRaw :: String -> String
substituteCircleNumsRaw = substituteCircleNums "{{[0-9]+}}"

substituteCircleNumsHTML :: String -> String
substituteCircleNumsHTML = substituteCircleNums "{{<span class=\"dv\">[0-9]+</span>}}"

mapPostBodies :: (String -> String) -> Item String -> Compiler (Item String)
mapPostBodies f post = do
  let body = itemBody post
  return $ post {itemBody = f body}

numberTagsInPosts :: Item String -> Compiler (Item String)
numberTagsInPosts = mapPostBodies (substituteCircleNumsHTML . substituteCircleNumsRaw)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route idRoute
        compile copyFileCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= numberTagsInPosts

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
                    constField "title" "Home"                `mappend`
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

