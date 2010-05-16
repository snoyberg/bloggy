module Model where

import Yesod
import qualified Data.ByteString as S
import System.Directory
import System.IO
import Data.Serialize
import Control.Arrow
import Data.List
import Data.Function
import Control.Monad
import Control.Applicative
import Data.Time
import System.Locale

loadEntry :: String -> IO (Maybe Entry)
loadEntry slug = do
    let fp = "entries/" ++ slug
    exists <- doesFileExist fp
    if exists
        then do
            withFile fp ReadMode $ \h -> do
                title <- S.hGetLine h
                date <- S.hGetLine h
                contents <- S.hGetContents h
                return $ Just Entry
                    { entrySlug = slug
                    , entryTitle = cs title
                    , entryDate = read $ cs date
                    , entryContent = Encoded $ cs contents
                    }
        else return Nothing

data Entry = Entry
    { entrySlug :: String
    , entryTitle :: String
    , entryDate :: Day
    , entryContent :: HtmlContent
    }

loadArchive :: IO Archive
loadArchive = do
    let fp = "archive.dat"
    exists <- doesFileExist fp
    ar <- if exists
            then do
                s <- S.readFile fp
                case decode s of
                    Left _ -> return Nothing
                    Right x -> return $ Just x
            else return Nothing
    case ar of
        Just x -> return x
        Nothing -> do
            x <- loadArchive'
            S.writeFile fp $ encode x
            return x

loadArchive' :: IO Archive
loadArchive' = do
    let top = "entries/"
    allContents <- getDirectoryContents top
    allFiles <- filterM (\f -> doesFileExist $ top ++ f) allContents
    pairs <- mapM (go top) allFiles
    return $ map (fst . head &&& map snd)
           $ groupBy ((==) `on` fst)
           $ map (first toYearMonth)
           $ reverse
           $ sortBy (compare `on` fst)
             pairs
  where
    go top f = do
        withFile (top ++ f) ReadMode $ \h -> do
            title <- S.hGetLine h
            date <- S.hGetLine h
            return (read $ cs date :: Day, EntryInfo f $ cs title)
    toYearMonth = formatTime defaultTimeLocale "%B %Y"

data EntryInfo = EntryInfo
    { eiSlug :: String
    , eiTitle :: String
    }
instance Serialize EntryInfo where
    put (EntryInfo a b) = put a >> put b
    get = EntryInfo <$> get <*> get

type YearMonth = String

type Archive = [(YearMonth, [EntryInfo])]

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%B %d, %Y"
