<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>$entry.title$ :: $bloggy.title$</title>
    <link rel="stylesheet" href="$bloggy.approot$static/style.css">
    <!--[if IE]>
      <script src="http://html5shiv.googlecode.com/svn/trunk/html5.js"></script>
    <![endif]-->
    <link rel="alternate" type="application/atom+xml" href="$bloggy.approot$feed/">
    <script src="http://cdn.jquerytools.org/1.1.2/full/jquery.tools.min.js"></script>
    <script>
        var initialMonth = "$entry.month$";
    </script>
    <script src="$bloggy.approot$static/script.js"></script>
  </head>
  <body>
    <section id="wrapper">
      <header>
        <h1>$bloggy.title$</h1>
        <h2>$bloggy.subtitle$</h2>
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
            <h4><div></div>$month.display$</h4>
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
        <h2 id="article-date">$entry.date$</h2>
        $entry.content$
      </article>
      <footer>
        Powered by <a href="http://www.yesodweb.com/code.html">Yesod Web Framework</a>
      </footer>
    </section>
  </body>
</html>
