<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>$entry.title$ :: $bloggy.title$</title>
    <link rel="stylesheet" href="$bloggy.static$style.css">
    <!--[if IE]>
      <script src="http://html5shiv.googlecode.com/svn/trunk/html5.js"></script>
    <![endif]-->
    <link rel="alternate" type="application/atom+xml" href="$bloggy.approot$feed/">
    <script src="http://cdn.jquerytools.org/1.1.2/full/jquery.tools.min.js"></script>
    <script>
        var approot = "$bloggy.approot$";
        var initialMonth = "$entry.month$";
        var currentSlug = "$entry.slug$";

        var discus_url = "$bloggy.approot$entry/$entry.slug$/";
var disqus_developer = 1;
    </script>
    <script src="$bloggy.static$script.js"></script>
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
        <h2 id="article-date">$entry.date$</h2>
        <section id="content">
          $entry.content$
        </section>
        <div id="disqus_thread"></div><script type="text/javascript" src="http://disqus.com/forums/snoyblog/embed.js"></script><noscript><a href="http://disqus.com/forums/snoyblog/?url=ref">View the discussion thread.</a></noscript><a href="http://disqus.com" class="dsq-brlink">blog comments powered by <span class="logo-disqus">Disqus</span></a>
      </article>
      <footer>
        Powered by <a href="http://www.yesodweb.com/code.html">Yesod Web Framework</a>
      </footer>
    </section>
<script type="text/javascript">
(function() {
	var links = document.getElementsByTagName('a');
	var query = '?';
	for(var i = 0; i < links.length; i++) {
	if(links[i].href.indexOf('#disqus_thread') >= 0) {
		query += 'url' + i + '=' + encodeURIComponent(links[i].href) + '&';
	}
	}
	document.write('<script charset="utf-8" type="text/javascript" src="http://disqus.com/forums/snoyblog/get_num_replies.js' + query + '"></' + 'script>');
})();
</script>
  </body>
</html>
