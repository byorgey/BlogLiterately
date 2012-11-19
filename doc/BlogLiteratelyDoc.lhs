[`BlogLiterately`][] is a tool for uploading blog posts to servers
that support the [MetaWeblog API][metaweblog] (such as
[WordPress][]-based blogs and many others).  Blog posts to be
published via `BlogLiterately` are written in [markdown][] format,
with
[extensions supported](http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown)
by [pandoc][].  Posts may be actual "bird-style" literate Haskell
files, with commentary in markdown.  Though `BlogLiterately` offers
special support for literate Haskell in particular, it is also useful
for writing posts including code written in other languages, or even
no code at all.

`BlogLiterately` includes support for syntax highlighting, $\LaTeX$
(including special support for WordPress blogs), and automatic
generation and formatting of `ghci` sessions.  Each of these features
is explained in more detail below.

Markdown and pandoc
-------------------

`BlogLiterately` takes as input files written using the [markdown][]
format.  See the [markdown website][markdown] for detailed
documentation.  `BlogLiterately` uses [pandoc][] for reading markdown,
which also [supports a few
extensions](http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown)
to the basic format.

Code blocks and syntax highlighting
-----------------------------------

Code segments (including actual source lines from literate haskell
files, as well as markdown code blocks) may be syntax highlighted.
Two different syntax highlighting libraries are supported:

  * [hscolour][] is specifically for syntax highlighting of Haskell
      code, and is the standard highlighter used on [Hackage][] and
      elsewhere.
  * [highlighting-kate][] is a general syntax highlighting library
      that can be used for highlighting a wide range of languages
      (including Haskell).

You may independently specify whether to use `hscolour` or
`highlighting-kate` to highlight Haskell code; other languages will be
highlighted with `highlighting-kate`.

In basic markdown, a generic code block is set off from normal text
by indenting at least four spaces:

        -- This is a code segment, but what language is it?
        foo :: String -> String

However, markdown has no way of specifying the language used in a code
block, making support for syntax highlighting problematic.  Pandoc
offers [an alternative
syntax](http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown)
for code segments which does allow specifying the language:

    ~~~~ { .haskell }
    -- This is a Haskell code segment!
    foo :: String -> String
    ~~~~

`BlogLiterately` also supports one additional style, consisting of a
normal markdown indented code block with an extra tag at the top,
enclosed in square brackets:

        [haskell]
        -- This is also a Haskell code segment!
        foo :: String -> String

Of course, languages other than Haskell may be specified as well.

By default, `hscolour` will be used for highlighting Haskell code,
using "inline" CSS style attributes.  The default styling is similar
to that used for source code in documentation on [Hackage][].  You can
also specify a configuration file containing a Haskell value of type
[(String,String)] which specifies a CSS style for each syntax
class. An example (corresponding to the default configuration) is
provided in the package archive (`hs-style`).

With `highlighting-kate`, the style for syntax segments is specified
using "class" attributes, so the stylesheet must be provided
separately.  You may optionally use a similar scheme with `hscolour`.
Sample stylesheets are provided in the package archive file
(`kate.css`, `hscolour.css`).

LaTeX
-----

LaTeX can be included in documents using single dollar signs to
enclose inline LaTeX, and double dollar signs to enclose
"display-style" LaTeX.  For example, `$\pi^2 / 6$` produces $\pi^2 /
6$, and `$$\sum_{k=0}^\infty 1/k^2$$` (when put by itself in its own
paragraph) produces

$$\sum_{k=0}^\infty 1/k^2.$$

Using the `--math` option, any
[Pandoc math rendering method may be chosen](http://johnmacfarlane.net/pandoc/README.html#math-rendering-in-html),
including MathML, jsMath, MathJax, and others.  Note that for some
methods to work properly, you may need to ensure that the generated
HTML ends up in the proper CSS or JavaScript environment. (What that
means depends on the method used.)

Alternatively, blogs hosted on
[wordpress.com](http://www.wordpress.com) have built-in
support for LaTeX, compiling LaTeX expressions to embedded images
on-the-fly.  Passing the `--wplatex` option to `BlogLiterately` causes
any embedded LaTeX to be output in the format expected by WordPress.
Note that an extra `$latex...` won't be added to the beginning of
LaTeX expressions which already appear to be in WordPress format.

`ghci` sessions
---------------

When writing literate Haskell documents, it is often useful to show a
sample `ghci` session illustrating the behavior of the code being
described.  However, manually pasting in the results of sample
sessions is tedious and error-prone, and it can be difficult keeping
sample sessions "in sync" when making changes to the code.

For these reasons, `BlogLiterately` supports special `[ghci]` code
blocks, consisting of a list of Haskell expressions (or, more
generally, arbitrary `ghci` commands), one per line.  These
expressions/commands are evaluated using `ghci`, and the results
typeset along with the original expressions in the output document.
The entire literate Haskell document itself will be loaded into `ghci`
before evaluating the expressions, so expressions may reference
anything in scope.  Note also that all expressions in the entire
document will be evaluated in the *same* `ghci` session, so names
bound with `let` or `<-` will also be in scope in subsequent
expressions, even across multiple `[ghci]` blocks.

For example, consider the following definition:

> hailstone x
>   | even x    = x `div` 2
>   | otherwise = 3*x + 1

Now, given the input

    [other]
        [ghci]
        :t hailstone
        hailstone 15
        takeWhile (/= 1) . iterate hailstone $ 7
        txt <- readFile "BlogLiteratelyDoc.lhs"
        length txt

`BlogLiterately` will generate the following output:

    [ghci]
    :t hailstone
    hailstone 15
    takeWhile (/= 1) . iterate hailstone $ 7
    txt <- readFile "BlogLiteratelyDoc.lhs"
    length txt

Additionally, lines indented by one or more space are interpreted as
*expected outputs* instead of inputs.  Consecutive indented lines are
interpreted as one multi-line expected output, with a number of spaces
removed from the beginning of each line equal to the number of spaces
at the start of the first indented line.

If the output for a given input is the same as the expected output (or
if no expected output is given), the result is typeset normally.  If
the actual and expected outputs differ, the actual output is typeset
first in red, then the expected output in blue.  For example,

    [other]
        [ghci]
        reverse "kayak"
        7+18
          25
        hailstone 15
          107834

produces

    [ghci]
    reverse "kayak"
    7+18
      25
    hailstone 15
      107834

There are currently a few known limitations of this feature:

* The code for interfacing with `ghci` is not very robust.  In
  particular, expressions which generate an error (*e.g.* ones which
  refer to an out-of-scope name, or do not typecheck) will simply lack
  any accompanying output; it would be much more useful to display the
  accompanying error message.

* If the literate document itself fails to load (*e.g.* due to
  improper formatting) `BlogLiterately` may hang.

* The formatting of `ghci` sessions currently cannot be
  customized.  Suggestions for customizations to allow are welcome.

* Due to the very hacky way that `ghci` interaction is implemented,
  the usual `it` variable bound to the result of the previous expression
  is not available (rather, it *is* available... but is always equal to
  `()`).

Uploading embedded images
-------------------------

When passed the `--upload-images` option, `BlogLiterately` can take
any images referenced locally and automatically upload them to the
server, replacing the local references with appropriate URLs.

To include images in blog posts, use the Markdown syntax

    ![alt text](URL "title")

The URL determines whether the image will be uploaded. A *remote* URL
is any beginning with `http` or a forward slash (interpreted as a URL
relative to the server root).  In all other cases it is assumed that
the URL in fact represents a relative path on the local file system.
Such images, if they exist, will be uploaded to the server (using the
`metaWeblog.newMediaObject` RPC call), and the local file name
replaced with the URL returned by the server.

Uploaded images, and their associated server URLs, will be tracked in
a file called `.BlogLiterately-uploaded-images`.  A given image will
only be uploaded once, even across multiple runs of `BlogLiterately`.
In practice, this means that the `--upload-images` option can be left
on while uploading multiple draft versions of a post, and only new
images will be uploaded each time.  Note, however, that images are
tracked by *file name*, not contents, so modifications to an image
(while leaving the name the same) will be ignored.  As a workaround,
delete `.BlogLiterately-uploaded-images` (or just the entry for the
modified image), or give the modified image a different name.

A few caveats:

* The `newMediaObject` call has an optional `replace` parameter, but
  `BlogLiterately` does not use it, since it's too dangerous: if
  `replace` is set and you happen to use the same file name as some
  other image file that already exists on your blog, the old image would
  be deleted.  However, this means that if you upload an image multiple
  times you will get multiple copies on your blog.  (Although this is mitigated
  somewhat by the mechanism to cache uploaded image URLs.)

Customization
-------------

It is possible to create your own variants of `BlogLiterately` which
include custom processing steps.  See the [`Text.BlogLiterately.Run`
module](http://hackage.haskell.org/packages/archive/BlogLiterately/latest/doc/html/Text-BlogLiterately-Run.html)
to get started.

Command-line options
--------------------

The options for `BlogLiterately` are hopefully self-explanatory, given the
above background:

    BlogLierately v0.4, (c) Robert Greayer 2008-2010, Brent Yorgey 2012
    This program comes with ABSOLUTELY NO WARRANTY

    BlogLiterately [OPTIONS] FILE

    Common flags:
      -s --style=FILE         style specification (for --hscolour-icss)
         --hscolour-icss      highlight haskell: hscolour, inline style (default)
         --hscolour-css       highlight haskell: hscolour, separate stylesheet
         --hs-nohighlight     no haskell highlighting
         --hs-kate            highlight haskell with highlighting-kate
         --other-kate         highlight other code with highlighting-kate
      -w --wplatex            reformat inline LaTeX the way WordPress expects
      -g --ghci               run [ghci] blocks through ghci and include output
         --upload-images      upload local images
         --category=ITEM      post category (can specify more than one)
         --tag=ITEM           tag (can specify more than one)
         --blogid=ID          Blog specific identifier
         --blog=URL           blog XML-RPC url (if omitted, html goes to stdout)
      -u --user=USER          user name
         --password=PASSWORD  password
      -t --title=TITLE        post title
         --postid=ID          Post to replace (if any)
         --page               create a "page" instead of a post (WordPress only)
         --publish            publish post (otherwise it's uploaded as a draft)
      -x --xtra=ITEM          extension arguments, for use with custom extensions
      -? --help               Display help message
      -V --version            Print version information

Example usage
-------------

If you do not specify a blog URL, by default `BlogLiterately` simply
prints the generated HTML to stdout.  So, to preview the generated
HTML before uploading requires merely something like

    BlogLiterately Sample.lhs

To actually post to, say, a WordPress blog, a basic command line would
be something like

    BlogLiterately --blog http://blogurl.example.com/xmlrpc.php \
        --user myname --password mypasswd --title "Sample" Sample.lhs

(which creates a new post).  You can also omit the `--password` field,
in which case `BlogLiterately` will prompt you for your password.

If the post id of that post (which `BlogLiterately` prints when it
uploads a new post) is '37', then to update the post, the command
would be something like

    BlogLiterately --postid 37 --blog http://blogurl.example.com/xmlrpc.php \
        --user myname --password mypasswd --title "Sample" Sample.lhs

and the post will be updated with the new text.  In both cases the
post is uploaded as a draft.  To publish the post, you can pass the
`--publish` option (or, of course, you can flip the publish bit
manually on the server).

Getting Help
------------

For questions, support, feature suggestions, etc., feel free to
contact me (Brent Yorgey): `byorgey` on IRC (freenode), or `byorgey`
at gmail.  There is also a [bug tracker][] where you can file bugs and
feature requests.

[`BlogLiterately`]: http://hackage.haskell.org/package/BlogLiterately
[markdown]: http://daringfireball.net/projects/markdown/
[pandoc]: http://johnmacfarlane.net/pandoc/
[hscolour]: http://www.cs.york.ac.uk/fp/darcs/hscolour/
[highlighting-kate]: http://johnmacfarlane.net/highlighting-kate/
[metaweblog]: http://www.xmlrpc.com/metaWeblogApi
[WordPress]: http://wordpress.org/
[Hackage]: http://hackage.haskell.org/
[MathML]: http://www.w3.org/Math/
[MathJax]: http://www.mathjax.org/
[bug tracker]: http://code.google.com/p/byorgey/issues/list?q=Project:BlogLiterately
