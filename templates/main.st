<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>$entry.title$ :: Snoyblog</title>
    <link rel="stylesheet" href="$approot$static/style.css">
    <!--[if IE]>
      <script src="http://html5shiv.googlecode.com/svn/trunk/html5.js"></script>
    <![endif]-->
  </head>
  <body>
    <section id="wrapper">
      <header>
        <h1>Michael Snoyman's Programming Thoughts</h1>
        <h2>Web development and Haskell, mostly</h2>
      </header>
      <nav>
        <h3>Blogroll</h3>
        <ul>
          <li><a href="http://www.yesodweb.com/">Yesod Web Development</a></li>
          <li><a href="http://www.snoyman.com/photos/">Family Photo Gallery</a></li>
          <li><a href="http://github.com/snoyberg/">Projects on Github</a></li>
        </ul>

        <h3>Archives</h3>
        <section id="archives">
          $archive:{ month |
            <h4>$month.display$</h4>
            <ul>
              $month.entries:{ entry |
                <li><a href="$entry.url$">$entry.title$</a></li>
              }$
            </ul>
          }$
        </section>
      </nav>
      <article>
        <h1 id="article-title">$entry.title$</h1>
        $entry.content$
      </article>
      <footer>
        Powered by <a href="http://www.yesodweb.com/code.html">Yesod Web Framework</a>
      </footer>
    </section>
  </body>
</html>
