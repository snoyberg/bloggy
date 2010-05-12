module Model where

import Yesod

loadEntry :: String -> IO (Maybe Entry)
loadEntry "foo" = return $ Just Entry
    { entrySlug = "foo"
    , entryTitle = "bar"
    , entryDate = "January 12, 2010"
    , entryContent = cs "It's my birthday!"
    }
loadEntry _ = return Nothing -- FIXME

data Entry = Entry
    { entrySlug :: String
    , entryTitle :: String
    , entryDate :: String
    , entryContent :: HtmlContent
    }

type YearMonth = String

loadArchive :: IO [(YearMonth, [EntryInfo])] -- FIXME use an enumerator
loadArchive = return [("January 2010", [EntryInfo "foo" "bar"])] -- FIXME

data EntryInfo = EntryInfo
    { eiSlug :: String
    , eiTitle :: String
    }
