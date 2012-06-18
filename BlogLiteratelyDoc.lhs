`BlogLiterately` is a tool for uploading blog posts to servers that
support the [MetaWeblog API][metaweblog] (such as [WordPress][]-based
blogs and many others).  Blog posts to be published via
`BlogLiterately` are written in [markdown][] format, with extensions
supported by [pandoc][].  Posts may be actual "bird-style" literate
Haskell files, with commentary in markdown.  Though `BlogLiterately`
offers special support for literate Haskell in particular, it is also
useful for writing posts including code written in other languages, or
even no code at all.

`BlogLiterately` includes support for syntax highlighting, inline
$\LaTeX$, automatic generation and formatting of `ghci` sessions,

Code blocks and syntax highlighting
-----------------------------------

Code segments (including actual source lines from literate haskell
files, as well as markdown code blocks) may be syntax highlighted.
XXX working here
There are two types (two different libraries used) of formatting
available for formatting code segments.

  * [hscolour][] (for formatting Haskell code segments)

  * highlighting-kate (for formatting Haskell and non-haskell segments)

The [Markdown website][markdown] has information about markdown
formatting options, and the [Pandoc website][pandoc] has information
about the extensions supported.  `BlogLiterately` extends the notation
a bit further, for specifying code segments.  In basic markdown, a
code segment is set off from normal text by indenting it at least four
spaces:

        -- This is a code segment but the tool doesn't know what kind!
        foo :: String -> String

Pandoc offers another way to specify a code segment:

~~~~~~~~~~~~~~~~~~~
~~~~ { .haskell }
-- This is a code segment, and the tool knows it's Haskell!
foo :: String -> String
~~~~
~~~~~~~~~~~~~~~~~~~

`BlogLiterately` lets you specify a Haskell segment this way (this is
just a normal markdown indented code block with an extra tag at
the top.  In either the above way of specifying the type of code
in the block, you may specify other kinds of code besides haskell,
e.g. cpp, bash, java, ml, eiffel, etc.):

    [other]
    [haskell]
    -- This is a code segment, and the tool knows it's Haskell!
    foo :: String -> String

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

Command-line options
--------------------

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
         --hscolour-icss    hilight haskell: hscolour, inline style (default)
         --hscolour-css     hilight haskell: hscolour, separate stylesheet
         --hs-nohilight     no haskell hilighting
         --hs-kate          hilight haskell with highlighting-kate
         --other-code-kate  hilight other code with highlighting-kate
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
[metaweblog]: http://www.xmlrpc.com/metaWeblogApi
[WordPress]: http://wordpress.org/
