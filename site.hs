--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Char    (isDigit, chr)
import           Data.Monoid  (mappend)
import           Hakyll
import qualified Text.RegexPR as RE
import qualified Text.Pandoc as TP
import Text.Pandoc (Pandoc)
import Text.Pandoc.Walk (walk)
import qualified Data.Text as T


--------------------------------------------------------------------------------
-- Drafts

isDraft :: Metadata -> Bool
isDraft meta =
  case lookupString "draft" meta of
    Nothing -> False
    Just s -> s == "true"

--------------------------------------------------------------------------------
-- CircleNum stuff

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
        digits = Prelude.filter isDigit s
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
-- Mermaidify stuff

-- | A Pandoc transformation that turns code blocks marked with the class "mermaid"
-- into raw HTML <div>s ready to be mermaidified by the browser with mermaid.js.
mermaidify :: Pandoc -> Pandoc
mermaidify = walk mermaidCodeBlock

-- | If the block contains the class "mermaid", then replace it with a <div>
-- containing the block's contents.
-- Note: extra attributes are currently ignored, but they shouldn't be hard
-- to take advantage of.
-- The syntax looks like this: ```{.mermaid myattr=myval}
-- which yields a `keyvals` of `[("myattr", "myval")]`
mermaidCodeBlock :: TP.Block -> TP.Block
mermaidCodeBlock (TP.CodeBlock (id, classes, keyvals) code) =
  if "mermaid" `elem` classes
    then TP.RawBlock "html" $ "<div caption=\"example thingy\" class=\"" <> T.unwords classes <> "\">" <> code <> "</div>"
    else TP.CodeBlock (id, classes, keyvals) code 
mermaidCodeBlock x = x

--------------------------------------------------------------------------------
-- Routes

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

    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= numberTagsInPosts

    matchMetadata "posts/*" (not . isDraft)
        postCompiler

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
                    metadataField                            `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
-- Route helpers

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCompiler :: Rules ()
postCompiler = do
  route $ setExtension "html"
  compile $ pandocCompilerWithTransform readerOptions writerOptions mermaidify
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
        >>= numberTagsInPosts
    where
      readerOptions = defaultHakyllReaderOptions
      writerOptions = defaultHakyllWriterOptions 
