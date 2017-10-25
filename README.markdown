[![Build Status](https://travis-ci.org/byorgey/BlogLiterately.svg?branch=master)](https://travis-ci.org/byorgey/BlogLiterately)

Write blog posts in Markdown format, then use BlogLiterately to do
syntax highlighting, format ghci sessions, and upload to any blog
supporting the [metaWeblog API (such as Wordpress)](http://codex.wordpress.org/XML-RPC_MetaWeblog_API).

To get started, use the provided executable `BlogLiterately`; see
[http://byorgey.wordpress.com/blogliterately/](http://byorgey.wordpress.com/blogliterately/)
for complete documentation.

To make further customization possible, the internals of the
executable are made available as a library.  In particular, it is easy
to create your own executable which adds extra custom transformations;
see [`Text.BlogLiterately.Run`](http://hackage.haskell.org/packages/archive/BlogLiterately/latest/doc/html/Text-BlogLiterately-Run.html).
