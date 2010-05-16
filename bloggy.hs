{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
import Yesod
import Yesod.Helpers.AtomFeed
import Yesod.Helpers.Static
import Model
import Data.Object
import Data.Object.Yaml
import Control.Monad
import Data.Time
import Data.Maybe
import Control.Applicative

-- | Normally we would want to load up entries in this, but I want it to run
-- best on CGI.
data Bloggy = Bloggy
    { bloggyStatic :: Static
    , bloggyApproot :: String
    , bloggyTitle :: String
    , bloggySubtitle :: String
    }

loadBloggy :: IO Bloggy
loadBloggy = do
    so <- join $ decodeFile "settings.yaml"
    m <- fromMapping so :: IO [(String, StringObject)]
    ar <- lookupScalar "approot" m :: IO String
    title <- lookupScalar "title" m
    subtitle <- lookupScalar "subtitle" m
    return Bloggy
        { bloggyStatic = fileLookupDir "static"
        , bloggyApproot = ar
        , bloggyTitle = title
        , bloggySubtitle = subtitle
        }

mkYesod "Bloggy" [$parseRoutes|
/                  MostRecentEntryR         GET
/entry/$slug       EntryR                   GET
/feed              FeedR                    GET
/static            StaticR                  Static siteStatic bloggyStatic
|]

instance Yesod Bloggy where
    approot = bloggyApproot

getMostRecentEntryR :: Handler Bloggy ()
getMostRecentEntryR = do
    a <- liftIO loadArchive
    redirect RedirectTemporary $ EntryR $ eiSlug $ head $ snd $ head a

getEntryR :: String -> Handler Bloggy RepHtml
getEntryR slug = do
    bloggy <- getYesod
    entry <- (liftIO $ loadEntry slug) >>= maybe notFound return
    archive <- liftIO loadArchive
    hamletToRepHtml [$hamlet|
!!!
%html
    %head
        %meta!charset=utf-8
        %title $cs.entryTitle.entry$ :: $cs.bloggyTitle.bloggy$
        %link!rel=stylesheet!href=@stylesheet@
        <!--[if IE]><script src="http://html5shiv.googlecode.com/svn/trunk/html5.js"></script><![endif]-->
        %link!rel=alternate!type=application/atom+xml!href=@FeedR@
        %script!src=$cs.jquerytools$
        %script!src=@script@
    %body
        %section#wrapper
            %header
                %h1 $cs.bloggyTitle.bloggy$
                %h2 $cs.bloggySubtitle.bloggy$
            %nav
                %h3 Blogroot
                %ul
                    %li
                        %a!href=$cs.yesodweb$ Yesod Web Framework
                    %li
                        %a!href=$cs.photos$ Family Photo Gallery
                    %li
                        %a!href=$cs.github$ Projects on Github
                %h3 Archives
                %section#archives
                $forall archive month
                    %h4 $cs.fst.month$
                    %ul
                        $forall snd.month entry
                            %li
                                %a!href=@EntryR.eiSlug.entry@
                                    $cs.eiTitle.entry$
            %article
                %h1#article-title $cs.entryTitle.entry$
                %h2#article-date $cs.showDay.entryDate.entry$
                %section#content
                    $entryContent.entry$
                <div id="disqus_thread"></div><script type="text/javascript" src="http://disqus.com/forums/snoyblog/embed.js"></script><noscript><a href="http://disqus.com/forums/snoyblog/?url=ref">View the discussion thread.</a></noscript><a href="http://disqus.com" class="dsq-brlink">blog comments powered by <span class="logo-disqus">Disqus</span></a>
            %footer
                Powered by 
                %a!href=$cs.yesodweb$ Yesod Web Framework
        <script type="text/javascript">(function() {var links = document.getElementsByTagName('a');var query = '?';for(var i = 0; i < links.length; i++) {if(links[i].href.indexOf('#disqus_thread') >= 0) { query += 'url' + i + '=' + encodeURIComponent(links[i].href) + '&'; } } document.write('<script charset="utf-8" type="text/javascript" src="http://disqus.com/forums/snoyblog/get_num_replies.js' + query + '"></' + 'script>'); })(); </script>
|]
  where
    stylesheet = StaticR $ StaticRoute ["style.css"]
    script = StaticR $ StaticRoute ["script.js"]
    jquerytools = "http://cdn.jquerytools.org/1.1.2/full/jquery.tools.min.js"
    yesodweb = "http://docs.yesodweb.com/"
    photos = "http://www.snoyman.com/photos/"
    github = "http://github.com/snoyberg"

getFeedR :: Handler Bloggy RepAtom
getFeedR = error "FIXME"

main :: IO ()
main = loadBloggy >>= toWaiApp >>= basicHandler 3000
