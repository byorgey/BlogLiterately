`BlogLiterately` is a tool for uploading blog posts to servers that
support the [MetaWeblog API][metaweblog] (such as [WordPress][]-based
blogs and many others).  Blog posts to be published via
`BlogLiterately` are written in [markdown][] format, with extensions
supported by [pandoc][].  Posts may be actual "bird-style" literate
Haskell files, with commentary in markdown.  Though `BlogLiterately`
offers special support for literate Haskell in particular, it is also
useful for writing posts including code written in other languages, or
even no code at all.

`BlogLiterately` includes support for syntax highlighting, $\LaTeX$
(including special support for WordPress blogs), automatic generation
and formatting of `ghci` sessions, and uploading embedded images.
Each of these features is explained in more detail below.

Markdown and pandoc
-------------------

`BlogLiterately` takes as input files written using the [markdown][]
format.  See the [markdown website][markdown] for detailed
documentation.  `BlogLiterately` uses [pandoc][] for
reading markdown, which also [supports a few extensions](XXX) to the
basic format.

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

In basic markdown, a generic code block is set off from normal text
by indenting at least four spaces:

        -- This is a code segment, but what language is it?
        foo :: String -> String

However, markdown has no way of specifying the language used in a code
block, making support for syntax highlighting problematic.  Pandoc
offers [an alternative syntax](XXX) for code segments which does allow
specifying the language:

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

XXX edit this

Once you have written your markdown file, you can run the tool,
specifying how you want it highlighted.  You can specify different
highlighting modes for the haskell segments and the other code
segments.  If using hscolour, you can specify that the highlighting be
done 'inline' via CSS 'style' attributes.  You can use the default
styling (which is similar to source code in documentation on hackage)
or you can specify a configuration file which is (readable as) a
Haskell value of type [(String,String)], and specifies a CSS style for
each syntax class. An example (corresponding to the default
configuration) is provided in the package archive (`hs-style`).

With highlighting-kate (always) and with hscolour (optionally), the
style for syntax segments is specified using 'class' attributes, so
the stylesheet must be provided separately.  Sample stylesheets are
provided in the package archive file (`kate.css`, `hscolour.css`).

XXX edit the above.

LaTeX
-----

LaTeX can be included in documents using single dollar signs to
enclose inline LaTeX, and double dollar signs to enclose
"display-style" LaTeX.  For example, `$\pi^2 / 6$` produces $\pi^2 /
6$, and `$$\sum_{k=0}^\infty 1/k^2$$` produces $$\sum_{k=0}^\infty
1/k^2.$$ By default, LaTeX is processed with Pandoc, meaning that a
certain subset of LaTeX expressions (such as those above) will be
transformed into [MathML][], and anything Pandoc cannot parse will be
passed through as literal LaTeX enclosed in dollar signs.

Blogs hosted on wordpress.com, however, have built-in support for
LaTeX, compiling LaTeX expressions to embedded images on-the-fly.
Passing the `--wplatex` option to `BlogLiterately` causes any embedded
LaTeX to be output in the format expected by WordPress (*e.g.* `$latex
\pi^2 / 6$`).  XXX mention `$latex...` won't be added if it already
exists.

XXX mention MathJax as future work.

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

Uploading embedded images
-------------------------

This feature is not yet implemented but is coming soon!

Command-line options
--------------------

XXX update to reflect the latest options.

The options for `BlogLiterately` are hopefully self-explanatory, given the
above background:

    BlogLierately v0.4, (C) Robert Greayer 2008-2010, Brent Yorgey 2012
    This program comes with ABSOLUTELY NO WARRANTY

    BlogLiterately [FLAG] URL USER PASSWORD TITLE FILE

      -? --help[=FORMAT]    Show usage information (optional format)
      -V --version          Show version information
      -v --verbose          Higher verbosity
      -q --quiet            Lower verbosity
      -t --test             do a test-run: html goes to stdout, is not posted
      -s --style=FILE       Style Specification (for --hscolour-icss)
         --hscolour-icss    highlight haskell: hscolour, inline style (default)
         --hscolour-css     highlight haskell: hscolour, separate stylesheet
         --hs-nohighlight   no haskell highlighting
         --hs-kate          highlight haskell with highlighting-kate
         --other-code-kate  highlight other code with highlighting-kate
         --publish          Publish post (otherwise it's uploaded as a draft)
         --category=VALUE   post category (can specify more than one)
      -b --blogid=VALUE     Blog specific identifier (default=default)
         --postid=VALUE     Post to replace (if any)

Example usage
-------------

To post to a WordPress blog, use something like:

    BlogLiterately http://blogurl.example.com/xmlrpc.php \
        myname mypasswd "Sample" Sample.lhs

(which creates a new post).  If, for example, the post id of that post
(which `BlogLiterately` prints when it uploads a new post) is '37', then
to update the post, the command would be:

    BlogLiterately --postid=37 http://blogurl.example.com/xmlrpc.php \
        myname mypasswd "Sample" Sample.lhs

and the post will be updated with the new text.


XXX can also be used as a simple filter with `-t`, either to preview
before uploading or as a simple standalone tool, XXX

[markdown]: http://daringfireball.net/projects/markdown/
[pandoc]: http://johnmacfarlane.net/pandoc/
[hscolour]: http://www.cs.york.ac.uk/fp/darcs/hscolour/
[highlighting-kate]: XXX
[metaweblog]: http://www.xmlrpc.com/metaWeblogApi
[WordPress]: http://wordpress.org/
[Hackage]: http://hackage.haskell.org/
[MathML]: XXX
