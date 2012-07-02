I have just released version 0.4 of [`BlogLiterately`][], a tool for
authoring and uploading blog posts (especially Haskelly ones).  Rob
Greayer authored the first few versions of this useful tool, for which
I'm very thankful.  However, he doesn't have the time or interest to
maintain it anymore, and I had a few ideas for extending and improving
it, so I offered to take over as the maintainer.

The [full (dog-fooded) documentation can be found
here](http://byorgey.wordpress.com/blogliterately/).  This blog post
is just to announce the release and show off a few capabilities.

Posts are written in [Markdown][] format (as recognized by
[pandoc][]), and `BlogLiterately` handles converting them to HTML and
uploading them to any blog that supports the [MetaWeblog
API][metaweblog] (such as WordPress). Haskell code can be syntax
highlighted using [hscolour][] (and any code can be syntax highlighted
using [highlighting-kate]):

> -- An awesome Haskell function
> fib :: Integer -> Integer
> fib 0 = 0
> fib 1 = 1
> fib n = fib (n-1) + fib (n-2)

Special support for WordPress LaTeX is built in: $\pi^2 / 6$.  `ghci`
sessions can be automatically generated from a list of inputs:

    [ghci]
    fib 20
    [1..10]

The one major planned feature that is still missing is uploading of
embedded images.  Sadly, this feature ran into a major roadblock in
the form of inexplicably closed HTTP connections (can you help [answer
my StackOverflow
question](http://stackoverflow.com/questions/11277788/errorclosed-exception-from-network-http-simplehttp-trying-to-upload-images-vi)?).
Ultimately my goal is to have completely automated support for
writing blog posts with inline [diagrams][] code.

Enjoy!

[`BlogLiterately`]: http://hackage.haskell.org/package/BlogLiterately
[Markdown]: http://daringfireball.net/projects/markdown/
[pandoc]: http://johnmacfarlane.net/pandoc/
[metaweblog]: http://www.xmlrpc.com/metaWeblogApi
[hscolour]: http://www.cs.york.ac.uk/fp/darcs/hscolour/
[highlighting-kate]: http://johnmacfarlane.net/highlighting-kate/
[diagrams]: http://projects.haskell.org/diagrams/