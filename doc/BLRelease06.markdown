% BlogLiterately 0.6

    [BLOpts]
    profile = wp
    postid = 1053

    tags = release, BlogLiterately, options
    categories = haskell, writing

I'm very proud to announce the release of
[`BlogLiterately` version 0.6](http://hackage.haskell.org/package/BlogLiterately-0.6),
a tool for formatting and uploading blog posts, including syntax
highlighting, generation of `ghci` sessions, LaTeX support, automatic
image uploading, and more.

**tl;dr**: Instead of cumbersomely specifying all options on the
command-line, you can now specify options using a combination of
"profiles" (*e.g.* for common sets of options such as blog URL and
password) and options embedded within the `.markdown` or `.lhs`
documents themselves (*e.g.* for post-specific options like title,
tags, and categories).

There are a few other changes and improvements as well.  For more
information, see the
[documentation](http://byorgey.wordpress.com/blogliterately/) or keep
reading below!

Specifying options
------------------

With previous releases, uploading a post usually went something like
this:

    BlogLiterately MyPost.md --blog "http://my.blog.url/xmlrpc.php \
      --user me --password 1234567 --postid 9999 --title "My awesome post" \
      --tag tag1 --tag tag2 --tag tag3 --category Stuff \
      --category OtherStuff --ghci --wplatex

which is incredibly tedious and error-prone.  Now we do things the
Right Way (tm).  First, you can create one or more *profiles*,
specifying a common set of options that can be referred to by name.
For example, you might have a profile for a particular blog, or a
profile for a particular type of post which always needs the same
options.  Suppose we put this in `$HOME/.BlogLiterately/foo.cfg` (or
in something like `C:/Documents And Settings/user/Application
Data/BlogLiterately/foo.cfg` on Windows):

    blog        = http://my.blog.url/xmlrpc.php
    user        = me
    password    = 1234567
    wplatex     = true

Now the previous command line is reduced to

    BlogLiterately MyPost.md -P foo --postid 9999 --title "My awesome post" \
      --tag tag1 --tag tag2 --tag tag3 --category Stuff \
      --category OtherStuff --ghci

which is already a big improvement!  But it doesn't stop there.  The
title, tags, categories, and other such things are really inherent to
the post itself; there's no reason they should go on the command line.
So, we add this indented block somewhere in `MyPost.md` (probably near
the top, though it doesn't matter):

    [other]
        [BLOpts]
        profile    = foo
        postid     = 9999
        title      = "My awesome post"
        tags       = tag1, tag2, tag3
        categories = Stuff, OtherStuff
        ghci       = true

And now we only have to write

    BlogLiterately MyPost.md

with no options on the command line at all!  Notice how we can even
specify which profile to use in the `[BLOpts]` block.  When we're
satisfied with the post we can publish it with

    BlogLiterately MyPost.md --publish

Generating HTML only
--------------------

In the past, to get a "preview" version of the HTML output written to
stdout, all you had to do was omit a `--blog` option.  However, if you
specify a profile with a `blog` field as in the above example, this is
more problematic.  For this reason, a new option `--html-only` has
been added.  When this option is specified, nothing will be uploaded,
and the HTML output written to stdout.

Changes to Transforms
---------------------

In order to make the above features possible, the definition of
`Transform` has changed.  This only affects those users who have
created their own custom transformations.  The definition used to be

   [haskell]
   data Transform
     = Transform
       { getTransform :: BlogLiterately -> Kleisli IO Pandoc Pandoc
       , xfCond       :: BlogLiterately -> Bool
       }

that is, a `Transform` was a transformation on `Pandoc` documents,
parameterized by an options record and able to have effects in the
`IO` monad.  The definition is now

    [haskell]
    data Transform
      = Transform
        { getTransform :: StateT (BlogLiterately, Pandoc) IO ()
        , xfCond       :: BlogLiterately -> Bool
        }

meaning that a `Transform` is able to transform *both* a `Pandoc`
document *and* the options record.  This is crucial for being able to
do things like embedding options within the document itself, because
we don't know all the options until we start processing the document!
Also, I switched from using `Kleisli` arrows to using `StateT`, since
I find it simpler to work with, especially now that multiple pieces of
state are involved.  For more information and help upgrading, see the
[documentation for `Text.BlogLiterately.Transform`](http://hackage.haskell.org/packages/archive/BlogLiterately/latest/doc/html/Text-BlogLiterately-Transform.html).

Move to github
--------------

The other change is that I have moved the `BlogLiterately` repository
from darcshub to github.  In general, for small personal projects and
miscellaneous sorts of things I use `darcs` and `hub.darcs.net`; for
larger projects where I want to raise the visibility and encourage
contributions from other users, I use github.  At some point
`BlogLiterately` crossed the line.

Learning more, and contacting me
--------------------------------

For more information, see the
[full documentation](http://byorgey.wordpress.com/blogliterately/).
I'm always happy to receive comments, questions, feature requests, bug
reports, and so on, via the
[bug tracker on github](https://github.com/byorgey/BlogLiterately/issues),
IRC (`byorgey` on freenode), or email (the same as my IRC nick, at gmail).
