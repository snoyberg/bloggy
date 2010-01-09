{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.AtomFeed
import Hack.Handler.SimpleServer
import Data.Object.Yaml
import Data.Object.Text
import Types
import Web.Encodings
import Data.Time
import System.Locale

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
        , ("date", cs $ formatTime defaultTimeLocale "%b %e, %Y"
                      $ entryDay e)
        ]

data Bloggy = Bloggy
    { blogTitle :: String
    , blogSubtitle :: String
    }
loadBloggy :: IO Bloggy
loadBloggy = return $ Bloggy "FIXME BLOG TITLE" "FIXME BLOG SUBTITLE"

instance Yesod Bloggy where
    handlers = [$resources|
/:
    Get: mostRecentEntryH
/entry/$entry:
    Get: showEntryH
/feed:
    Get: showFeedH
/static/*filepath: serveStatic'
|]
    templateDir _ = "templates"
instance YesodApproot Bloggy where
    approot _ = Approot "http://localhost:3000/" -- FIXME

serveStatic' :: Verb -> [String] -> Handler y [(ContentType, Content)]
serveStatic' = serveStatic $ fileLookupDir "static"

slugToUrl y s =
    let (Approot ar) = approot y
     in ar ++ "entry/" ++ encodeUrl s ++ "/"

loadArchive :: IO Archive
loadArchive = readYamlDoc archiveFile >>= convertAttemptWrap

showEntry :: Entry -> Handler Bloggy Template
showEntry e = do
    y <- getYesod
    template "main" "entry" (cs e) $ do
        archive <- loadArchive
        return
            [ ("bloggy", cs y)
            , ("archive", cs (slugToUrl y, archive))
            ]

instance ConvertSuccess Bloggy HtmlObject where
    convertSuccess b = cs
        [ ("approot", unApproot $ approot b)
        , ("title", blogTitle b)
        , ("subtitle", blogSubtitle b)
        ]

--readEntry :: MonadIO m => String -> m Entry
readEntry s = do
    contents <- liftIO $ readFile $ entriesDir ++ s
    let (t:d':rest) = lines contents
    let content = unlines rest
    d <- convertAttemptWrap d'
    return $ Entry (cs t) (Html $ cs content) s d

showEntryH :: String -> Handler Bloggy Template
showEntryH eSlug = readEntry eSlug >>= showEntry

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
main = putStrLn "Running..." >> loadBloggy >>= run 3000 . toHackApp
