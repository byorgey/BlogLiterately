    [BLOpts]
    profile = wp
    postid  = 802
    title   = BlogLiterately
    ghci    = true
    wplatex = true
    page    = true
    publish = true
    toc     = true

[`BlogLiterately`][] is a tool for uploading blog posts to servers
that support the [MetaWeblog API][metaweblog] (such as
[WordPress][]-based blogs and many others).  Blog posts to be
published via `BlogLiterately` are written in [markdown][] or
[reStructuredText][] format, with [extensions
supported](http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown)
by [pandoc][].  Posts may be actual "bird-style" literate Haskell
files, with commentary formatted using markdown or reStructuredText.
Though `BlogLiterately` offers special support for literate Haskell in
particular, it is also useful for writing posts including code written
in other languages, or even no code at all.  You may also be
interested in the [BlogLiterately-diagrams][] package, a plugin for
`BlogLiterately` which allows embedding images in your posts defined
using the [diagrams][] vector graphics framework.

`BlogLiterately` includes support for syntax highlighting, $\LaTeX$
(including special support for WordPress blogs), automatic image
uploading, and automatic generation and formatting of `ghci` sessions.
Each of these features is explained in more detail below.

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

(which creates a new post).  You can also omit the `--password` option,
in which case `BlogLiterately` will prompt you for your password.

If the post ID of that post (which `BlogLiterately` prints when it
uploads a new post) is '37', then to update the post, the command
would be something like

    BlogLiterately --postid 37 --blog http://blogurl.example.com/xmlrpc.php \
        --user myname --password mypasswd --title "Sample" Sample.lhs

and the post will be updated with the new text.  In both cases the
post is uploaded as a draft.  To publish the post, you can pass the
`--publish` option (or, of course, you can flip the publish bit
manually on the server).

The above examples only show the most basic usage. In particular, the
pain of constructing long command lines like the above is unnecessary,
and can be replaced by the use of profiles and embedding options
within the source file itself; these features are explained below.

Markdown and pandoc
-------------------

`BlogLiterately` can take as input files written using the
[markdown][] format (as well as [reStructuredText][]).  See the
[markdown website][markdown] for detailed documentation.
`BlogLiterately` uses [pandoc][] for reading markdown, which also
[supports a few extensions](http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown)
to the basic format.

By default, `BlogLiterately` assumes that markdown files should be
parsed as if they contain literate Haskell code.  To disable
processing of markdown files with literate Haskell extensions, use the
`--no-lit-haskell` command-line argument.  This makes a difference,
for example, when processing paragraphs set off by "bird tracks"
(*i.e.* leading `>` characters): in literate Haskell, these are code
blocks, whereas in plain markdown they are blockquotes.  In addition,
section headings beginning with hash signs (`#`, `##`, etc.) cannot be
used in literate Haskell mode; only section headings underlined with
hyphens or equals signs are supported.

Determining input format
------------------------

`BlogLiterately` takes the following steps to determine whether an
input file is in markdown or reStructuredText format:

  1. If the format is explicitly specified on the command line with
     `--format=markdown` or `--format=rst`, the specified format will be
     used regardless of the file name.

  2. Otherwise, the filename extension is consulted: if it is `.rst`,
     `.rest`, or `.txt`, reStructuredText will be assumed; otherwise,
     markdown is assumed.

Code blocks and syntax highlighting
-----------------------------------

Code segments (including actual source lines from literate haskell
files, as well as markdown or reStructuredText code blocks) may be
syntax highlighted.  Two different syntax highlighting libraries are
supported:

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

Similarly, in reStructuredText, a code block is constructed by a double colon
followed by an indented block:

    ::
    
      -- This is a code segment, but what language is it?
      foo :: String -> String

However, markdown does not have a way of specifying the language used
in a code block, making support for syntax highlighting problematic.
Pandoc offers
[an alternative syntax](http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown)
for code segments which does allow specifying the language:

    ~~~~ { .haskell }
    -- This is a Haskell code segment!
    foo :: String -> String
    ~~~~

The above syntax works only with markdown. `BlogLiterately` also
supports one additional style which works with both markdown and
reStructuredText, consisting of a normal code block (indented and/or
preceded by a double colon) with an extra tag at the top, enclosed in
square brackets:

        [haskell]
        -- This is also a Haskell code segment!
        foo :: String -> String

Of course, languages other than Haskell may be specified as well.

By default, `hscolour` will be used for highlighting Haskell code,
using "inline" CSS style attributes.  The default styling is similar
to that used for source code in documentation on [Hackage][].  You can
also specify a configuration file containing a Haskell value of type
`[(String,String)]` which specifies a CSS style for each syntax
class. An example (corresponding to the default configuration) is
provided in the package archive (`hs-style`).

With `highlighting-kate`, the style for syntax segments is specified
using "class" attributes, so the stylesheet must be provided
separately.  You may optionally use a similar scheme with `hscolour`.
Sample stylesheets are provided in the package archive file
(`kate.css`, `hscolour.css`).

Citations
---------

`BlogLiterately` can take advantage of `pandoc`'s ability to process
and typeset citations.  To include citations in your blog post:

1. Specify a bibliography---either the name of a bibliography file, or
   an explicit list of references---as metadata in your document.  With
   Markdown, this is accomplished with a YAML document enclosed by `---`
   at the beginning of the file (see the Pandoc documentation on [YAML
   metadata
   blocks](http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html#yaml-metadata-block)).
   For example,

         ---
         title: My Blog Post
         bibliography: references.bib
         ---
         Foo bar [@doe2006].

   (There is no support yet for citations if you are using
   reStructuredText; yell if you want it.)  You can specify the name of a
   file containing a bibliography, as in the example above; here is [a
   list of the bibliography formats that are
   accepted](https://github.com/jgm/pandoc-citeproc/blob/master/README.md).
   Alternately, you can [give an explicit list of references](http://johnmacfarlane.net/pandoc/README.html#citations).

2. Include citations, formatted like `[@doe2006]` for a normal
   citation like (Doe, 2006); `@doe2006` for a text citation like Doe
   (2006), or `[-@doe2006]` for a citation without the name (for
   situations when the name already occurred elsewhere in the sentence).
   See the [pandoc documentation for more details and
   examples](http://johnmacfarlane.net/pandoc/README.html#citations).

3. Simply run `BlogLiterately`; citation processing is on by
   default. (You can explicitly turn it on with the `--citations` flag;
   to turn it off, use `--no-citations`.)  Citations will be typeset and
   a bibliography will be appended at the end. You may want to include a
   section heading like `# References` or `# Bibliography` at the end of
   your post, to go above the generated bibliography.

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

Finally, to simply pass LaTeX math through unchanged (for example, if
your blog hosting software will do LaTeX processing), you can use the
`--rawlatex` option.

Special links
-------------

Certain special link types can be replaced with appropriate URLs.  A
special link is one where the URL is of the form `<name>::<text>`
where `<name>` is used to identify the special link type, and `<text>`
is passed as a parameter to a function which can use it to generate a
URL. Currently, four types of special links are supported by default
(and you can easily add your own):

`lucky::<search>`

:   The first Google result for `<search>`.

`wiki::<title>`

:   The Wikipedia page for `<title>`.  (Note that the page is not
    checked for existence.)

`post::nnnn`

:   Link to the blog post on your blog with post ID `nnnn`.  Note that
    this form of special link is invoked when `nnnn` consists of all
    digits, so it only works on blogs which use numerical identifiers for
    post IDs (as Wordpress does).

`post::<search>`

:   Link to the most recent blog post (among the
    20 most recent posts) containing `<search>` in its title.

For example, a post written in Markdown format containing

```
    This is a post about the game of [Go](wiki::Go (game)).
```

will be formatted in HTML as

```
    <p>This is a post about the game of <a href="https://en.wikipedia.org/wiki/Go%20(game)">Go</a>.</p>
```

You can easily add your own new types of special links.  See the
`SpecialLink` type and the `mkSpecialLinksXF` function.

Table of contents
-----------------

`BlogLiterately` can also take advantage of pandoc's ability to
generate a table of contents.  Just pass the `--toc` option to
`BlogLiterately` and a table of contents will be added to the top of
your post.  See this documentation itself for an example of the
output.

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

        [ghci]
        :t hailstone
        hailstone 15
        takeWhile (/= 1) . iterate hailstone $ 7
        txt <- readFile "BlogLiteratelyDoc.lhs"
        length txt

`BlogLiterately` generates the following output:

    [ghci]
    :t hailstone
    hailstone 15
    takeWhile (/= 1) . iterate hailstone $ 7
    txt <- readFile "BlogLiteratelyDoc.lhs"
    length txt

(And yes, of course, the above output really *was* generated by
BlogLiterately!)  Additionally, lines indented by one or more spaces
are interpreted as *expected outputs* instead of inputs.  Consecutive
indented lines are interpreted as one multi-line expected output, with
a number of spaces removed from the beginning of each line equal to
the number of spaces at the start of the first indented line.

If the output for a given input is the same as the expected output (or
if no expected output is given), the result is typeset normally.  If
the actual and expected outputs differ, the actual output is typeset
first in red, then the expected output in blue.  For example,

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
  the usual `it` variable bound to the result of the previous
  expression is not available (well, to be more precise, it *is*
  available... but is always equal to `()`).

Uploading embedded images
-------------------------

When passed the `--upload-images` option, `BlogLiterately` can take
any images referenced locally and automatically upload them to the
server, replacing the local references with appropriate URLs.

To include images in blog posts, use the Markdown syntax

    ![alt text](URL "title")

(or the corresponding reStructuredText syntax).

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

Most of the command-line options for `BlogLiterately` are hopefully
self-explanatory, given the above background:

    BlogLierately v0.7, (c) Robert Greayer 2008-2010, Brent Yorgey 2012-2013
    For help, see http://byorgey.wordpress.com/blogliterately/

    BlogLiterately [OPTIONS] FILE

    Common flags:
      -s --style=FILE         style specification (for --hscolour-icss)
         --hscolour-icss      highlight haskell: hscolour, inline style (default)
         --hscolour-css       highlight haskell: hscolour, separate stylesheet
         --hs-nohighlight     no haskell highlighting
         --hs-kate            highlight haskell with highlighting-kate
         --kate               highlight non-Haskell code with highlighting-kate
                              (default)
         --no-kate            don't highlight non-Haskell code
         --lit-haskell        parse as literate Haskell (default)
         --no-lit-haskell     do not parse as literate Haskell
         --no-toc             don't generate a table of contents (default)
         --toc                generate a table of contents
      -r --rawlatex           pass inline/display LaTeX through unchanged
      -w --wplatex            reformat inline LaTeX the way WordPress expects
      -m --math=ITEM          how to layout math, where
                              --math=<pandoc-option>[=URL]
      -g --ghci               run [ghci] blocks through ghci and include output
      -I --upload-images      upload local images
      -C --category=ITEM      post category (can specify more than one)
      -T --tag=ITEM ---tags   tag (can specify more than one)
         --blogid=ID          Blog specific identifier
      -P --profile=STRING     profile to use
      -b --blog=URL           blog XML-RPC url (if omitted, HTML goes to stdout)
      -u --user=USER          user name
      -p --password=PASSWORD  password
      -t --title=TITLE        post title
      -f --format=FORMAT      input format: markdown or rst
      -i --postid=ID          Post to replace (if any)
         --page               create a "page" instead of a post (WordPress only)
         --publish            publish post (otherwise it's uploaded as a draft)
      -h --html-only          don't upload anything; output HTML to stdout
         --citations          process citations (default)
         --no-citations       do not process citations
      -x --xtra=ITEM          extension arguments, for use with custom extensions
      -? --help               Display help message
      -V --version            Print version information
         --numeric-version    Print just the version number

Profiles
--------

Certain options, such as `--blog`, `--user`, and `--wplatex`, may be
the same for all your posts.  You can create one or more *profiles*
specifying a set of options, which can then be specified simply by
referencing the profile, using the command-line option
`--profile`/`-P`.  For example, to use the profile named `foo` you
would invoke

    BlogLiterately -P foo ...

(Alternately, you can also specify `profile = foo` within a `[BLOpts]`
block in the source file itself; see the next section.)

The profile *foo* should be stored in a file named `foo.cfg`, and
placed in the application directory for `BlogLiterately`: on POSIX
systems, this means `$HOME/.BlogLiterately/foo.cfg`; on Windows, it
typically means something like `C:/Documents And
Settings/user/Application Data/BlogLiterately/foo.cfg`.

The profile should consist of a number of options, listed one per
line, in the form

    optionname = value

Boolean options are specified by `true`, `on`, `false`, or `off`.
String values use normal Haskell syntax for strings, surrounded by
double quotes. Optionally, the double quotes may be omitted for
strings which do not contain spaces, double quotes, commas, or square
brackets. Lists also use Haskell list syntax, with comma-separated
items surrounded by square brackets, except that the square brackets
may be omitted.  For example, `myblog.cfg` might look like this:

    blog       = http://some.url/xmlrpc.php
    user       = joebloggs
    password   = f7430nvj!$4
    wplatex    = true
    ghci       = on
    categories = foo, bar, "some really long category"

The list of options which are currently supported are: `style`,
`lit-haskell`, `wplatex`, `math`, `ghci`, `upload-images`,
`categories`, `tags`, `blogid`, `profile`, `blog`, `user`, `password`,
`title`, `postid`, `page`, `publish`, `xtras`.

Option blocks
-------------

In addition, options may be specified inline, using a code block
marked with the `[BLOpts]`.  For example,

        [BLOpts]
        profile = foo
        title = "My awesome blog post!"
        postid = 2000
        tags = [awesome, stuff, blogging]
        categories = [Writing, Stuff]

    This is my awesome blog post. Here is some math: $\pi$, which will
    get formatted for WordPress because I chose the `foo` profile
    above, which includes `wplatex = true`.

Such inline options use the same syntax as profiles, as described in
the previous section.

Pandoc titles
-------------

Pandoc supports a special syntax for specifying the title, placing the
title on the first line marked with `%`.  `BlogLiterately` supports
this format too, so the above example could also have been written as:

    % My awesome blog post!

        [BLOpts]
        profile = foo
        postid = 2000
    ...

Generating HTML only
--------------------

In the past, to get a "preview" version of the HTML output written to
stdout, all you had to do was omit a `--blog` option.  However, if you
specify a profile with a `blog` field, this is more problematic.  For
this reason, a new option `--html-only` has been added.  When this
option is specified, nothing is uploaded, and the HTML output is
written to stdout.

Getting Help
------------

For questions, support, feature suggestions, etc., feel free to
contact me (Brent Yorgey): `byorgey` on IRC (freenode), or `byorgey`
at gmail.  There is also a [bug tracker][] where you can file bugs and
feature requests.

[`BlogLiterately`]: http://hackage.haskell.org/package/BlogLiterately
[markdown]: http://daringfireball.net/projects/markdown/
[reStructuredText]: http://docutils.sourceforge.net/docs/user/rst/quickref.html
[BlogLiterately-diagrams]: http://hackage.haskell.org/package/BlogLiterately%2Ddiagrams
[diagrams]: http://projects.haskell.org/diagrams/
[pandoc]: http://johnmacfarlane.net/pandoc/
[hscolour]: http://www.cs.york.ac.uk/fp/darcs/hscolour/
[highlighting-kate]: http://johnmacfarlane.net/highlighting-kate/
[metaweblog]: http://www.xmlrpc.com/metaWeblogApi
[WordPress]: http://wordpress.org/
[Hackage]: http://hackage.haskell.org/
[MathML]: http://www.w3.org/Math/
[MathJax]: http://www.mathjax.org/
[bug tracker]: http://github.com/byorgey/BlogLiterately/issues
