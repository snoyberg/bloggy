{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Yesod
import Yesod.Helpers.Static
import Hack.Handler.SimpleServer
import Data.Object.Yaml
import Data.Object.Text
import Types
import Web.Encodings
import Data.Time
import System.Locale

data Entry = Entry Text Text Day
instance ConvertSuccess Entry HtmlObject where
    convertSuccess (Entry t c d) = cs
        [ ("title", Text t)
        , ("content", Html c)
        , ("date", cs $ formatTime defaultTimeLocale "%b %e, %Y" d)
        ]

data Bloggy = Bloggy
loadBloggy :: IO Bloggy
loadBloggy = return Bloggy

instance Yesod Bloggy where
    handlers = [$resources|
/:
    Get: mostRecentEntryH
/entry/$entry:
    Get: showEntryH
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
    let (Approot ar) = approot y
    template "main" "entry" (cs e) $ do
        archive <- loadArchive
        return
            [ ("approot", cs ar)
            , ("archive", cs (slugToUrl y, archive))
            ]

--readEntry :: MonadIO m => String -> m Entry
readEntry s = do
    contents <- liftIO $ readFile $ entriesDir ++ s
    let (t:d':rest) = lines contents
    let content = unlines rest
    d <- convertAttemptWrap d'
    return $ Entry (cs t) (cs content) d

showEntryH :: String -> Handler Bloggy Template
showEntryH eSlug = readEntry eSlug >>= showEntry

mostRecentEntryH = do
    ((_, (ei:_)):_) <- liftIO loadArchive
    showEntryH $ slug ei

main :: IO ()
main = putStrLn "Running..." >> loadBloggy >>= run 3000 . toHackApp
