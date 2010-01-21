{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import Data.Object.Html
import Data.Object.Yaml
import Data.Object.Text
import Data.Attempt
import Control.Monad
import qualified Safe.Failure as SF

entriesDir = "entries/"
archiveFile = "archive.yaml"

data Month = January | February | March | April | May | June
           | July | August | September | October | November | December
    deriving (Enum, Bounded, Eq, Show, Read, Ord)
data YearMonth = YearMonth Int Month
    deriving (Eq, Show, Read, Ord)
data EntryInfo = EntryInfo { slug :: String, title :: String }
    deriving (Eq, Show, Read, Ord)
type Archive = [(YearMonth, [EntryInfo])]
type SlugToUrl = String -> String
instance ConvertSuccess (SlugToUrl, Archive) HtmlObject where
    convertSuccess (stourl, a) = Sequence $ map helper a where
        helper :: (YearMonth, [EntryInfo]) -> HtmlObject
        helper (YearMonth y m, eis) = cs
            [ ("display", cs $ show m ++ " " ++ show y)
            , ("entries", Sequence $ map helper2 eis)
            ]
        helper2 :: EntryInfo -> HtmlObject
        helper2 ei = cs
            [ ("url", stourl $ slug ei)
            , ("title", title ei)
            ]

archiveToTextObject :: Archive -> TextObject
archiveToTextObject = Sequence . map helper where
    helper :: (YearMonth, [EntryInfo]) -> TextObject
    helper (YearMonth y m, eis) = cs
        [ ("year", toTextObject $ show y)
        , ("month", cs $ show m)
        , ("entries", Sequence $ map helper2 eis)
        ]
    helper2 ei = cs
        [ ("slug", slug ei)
        , ("title", title ei)
        ]

archiveFromTextObject :: FromAttempt m => TextObject -> m Archive
archiveFromTextObject to = fa $ do
    seq <- fromSequence to
    mapM helper seq
      where
        helper :: TextObject -> Attempt (YearMonth, [EntryInfo])
        helper to = do
            ma <- fromMapping to
            y <- lookupObject "year" ma
            mo' <- lookupObject "month" ma
            mo <- SF.read mo'
            es <- lookupObject "entries" ma
            return (YearMonth y mo, es)

instance ConvertAttempt TextObject [EntryInfo] where
    convertAttempt = mapM helper <=< fromSequence where
        helper to = do
            ma <- fromMapping to
            s <- lookupObject "slug" ma
            t <- lookupObject "title" ma
            return $ EntryInfo s t
