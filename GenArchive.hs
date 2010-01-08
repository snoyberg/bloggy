import System.Directory
import Types
import Data.Object.Html
import Data.Object.Yaml
import Data.List
import Data.Time
import Control.Monad
import Data.Function
import Data.Function.Predicate

notHidden ('.':_) = False
notHidden _ = True

main = do
    allContents <- getDirectoryContents entriesDir
    allFiles <- filterM (\e -> doesFileExist $ entriesDir ++ e) allContents
    let files = filter notHidden allFiles
    pairs <- mapM readEntry files
    print $ sort pairs
    print $ reverse $ sort pairs
    let archive = map hoist $ groupBy ((==) `on` fst) $ reverse $ map snd $ sort pairs
    writeYamlDoc archiveFile $ cs (archive :: Archive)

readEntry :: FilePath -> IO (Day, (YearMonth, EntryInfo))
readEntry fp = do
    (t:d:_) <- lines `fmap` readFile (entriesDir ++ fp)
    d' <- convertAttemptWrap d
    let (y, m, _) = toGregorian d'
    return (d', (YearMonth (fromIntegral y) $ toEnum $ m - 1, EntryInfo fp t))

hoist :: [(k, v)] -> (k, [v])
hoist x = (fst $ head x, map snd x)
