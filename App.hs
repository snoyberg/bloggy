{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.AtomFeed
import Network.Wai.Handler.SimpleServer
import Data.Object.Yaml
import Data.Object
import Data.Attempt
import Types
import Archive
import Web.Encodings
import Data.Time
import System.Locale
import Control.Monad

data Entry = Entry
    { entryTitle :: String
    , entryContent :: Html
    , entrySlug :: String
    , entryDay :: Day
    }
instance ConvertSuccess Entry HtmlObject where
    convertSuccess e = cs
        [ ("title", cs $ entryTitle e)
        , ("content", entryContent e)
        , ("date", cs $ formatTime defaultTimeLocale "%B %e, %Y"
                      $ entryDay e)
        , ("month", cs $ formatTime defaultTimeLocale "%B %Y" $ entryDay e)
        , ("slug", cs $ entrySlug e)
        ]

data Bloggy = Bloggy
    { blogTitle :: String
    , blogSubtitle :: String
    , blogApproot :: String
    , blogStatic :: String
    , blogTG :: TemplateGroup
    }
loadBloggy :: IO Bloggy
loadBloggy = do
    so <- decodeFile "settings.yaml"
    tg <- loadTemplateGroup "templates"
    fa $ helper so tg
      where
        helper :: StringObject -> TemplateGroup -> Attempt Bloggy
        helper so tg = do
            m <- fromMapping so
            t <- lookupObject "title" m
            s <- lookupObject "subtitle" m
            a <- lookupObject "approot" m
            st <- lookupObject "static" m
            return $ Bloggy t s a st tg

instance Yesod Bloggy where
    resources = [$mkResources|
/:
    GET: mostRecentEntryH
/entry/$entry:
    GET: showEntryH
/feed:
    GET: showFeedH
/static/*filepath: serveStatic'
|]
instance YesodApproot Bloggy where
    approot = blogApproot
instance YesodTemplate Bloggy where
    getTemplateGroup = blogTG
    defaultTemplateAttribs _ _ = return

serveStatic' :: Method -> [String] -> Handler y [(ContentType, Content)]
serveStatic' = serveStatic $ fileLookupDir "static"

slugToUrl y s = approot y ++ "entry/" ++ encodeUrl s ++ "/"

showEntry :: Entry -> Handler Bloggy RepHtmlJson
showEntry e = do
    y <- getYesod
    templateHtmlJson "main" (cs e) $ \ho t -> do
        archive <- loadArchive
        return $ setHtmlAttrib "bloggy" y
               $ setHtmlAttrib "archive" (slugToUrl y, archive)
               $ setHtmlAttrib "entry" e
                 t

instance ConvertSuccess Bloggy HtmlObject where
    convertSuccess b = cs
        [ ("approot", approot b)
        , ("title", blogTitle b)
        , ("subtitle", blogSubtitle b)
        , ("static", blogStatic b)
        ]

--readEntry :: MonadIO m => String -> m Entry
readEntry s = do
    contents <- liftIO $ readFile $ entriesDir ++ s
    let (t:d':rest) = lines contents
    let content = unlines rest
    d <- convertAttemptWrap d'
    return $ Entry (cs t) (Html $ cs content) s d

showEntryH :: String -> Handler Bloggy RepHtmlJson
showEntryH eSlug = liftIO (readEntry eSlug) >>= showEntry

mostRecentEntryH = do
    ((_, (ei:_)):_) <- liftIO loadArchive
    showEntryH $ slug ei

showFeedH = do
    archive <- liftIO loadArchive
    let slugs = take 10 $ map slug $ concatMap snd archive
    entries@(firstEntry:_) <- liftIO $ mapM readEntry slugs
    y <- getYesod
    atomFeed $ AtomFeed
        (blogTitle y)
        (RelLoc "feed/")
        (RelLoc "")
        (cs $ entryDay firstEntry)
        (map cs entries)

instance ConvertSuccess Entry AtomFeedEntry where
    convertSuccess e = AtomFeedEntry
        (RelLoc $ "entry/" ++ encodeUrl (entrySlug e) ++ "/")
        (cs $ entryDay e)
        (cs $ entryTitle e)
        (entryContent e)

instance ConvertSuccess Day UTCTime where
    convertSuccess d = UTCTime d $ secondsToDiffTime 0

main :: IO ()
main = putStrLn "Running..." >> loadBloggy >>= toWaiApp >>= run 3000
