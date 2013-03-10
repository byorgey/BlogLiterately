0.6: 10 March 2013
------------------

  * Add support for "profiles" with sets of common options

  * Add support for reading options from inline blocks tagged `[BLOpts]`

  * Add support for reading post titles using pandoc-supported title
    block format, `% Title`

  * Transforms are now of type StateT (BlogLiterately, Pandoc) IO (),
    to allow transforms to alter the options record as well as the
    document

  * Add `centerImagesXF` to standard transforms

  * Move a bunch of ad-hoc functionality into standard transforms

  * Add `--html-only` option

  * bump `pandoc` upper bound to < 1.12

0.5.4.1: 18 February 2013
-------------------------

  * bump `blaze-html` upper bound to < 0.7

0.5.4: 24 January 2013
----------------------

  * Require `pandoc` 1.10.

0.5.3: 19 November 2012
-----------------------

  * New `--math` option for selecting pandoc math writing mode

  * Run the pandoc parser in "smart" mode, which generates proper en-
    and em-dashes, quotation marks, etc.

  * More updates for GHC 7.6.1 compatibility

  * Using `highlighting-kate` to highlight non-Haskell code was already
    the default; make this more clear.

    The `--other-kate` option is no more; now there are two options

      `--kate`
      `--no-kate`

    with `--kate` the default.

0.5.2.1: 19 September 2012
--------------------------

  * bump `base` upper bound to <4.7

0.5.2: 20 August 2012
---------------------

  * improvement to behavior of `--upload-images` flag: cache uploaded
    image server URLs, even across multiple runs, to avoid uploading
    the same image multiple times

  * bump dep upper bounds:
    - `split` to < 0.3
    - `cmdargs` to < 0.11

0.5.1: 30 July 2012
-------------------

  * Escape `<` and `>` characters in `ghci` output

  * Supress vertical whitespace following `ghci` commands that produce
    no output

  * add `centerImagesXF` transform (disabled by default)

  * create bug tracker and add `Bug-reports:` field to `.cabal` file

  * re-export `Text.BlogLiterately.Run` from `Text.BlogLiterately`

  * improved documentation

  * fix output of `--version`

0.5: 7 July 2012
----------------

  * expose internals as a library, and create framework for adding
    custom transformations to the pipeline

  * image uploads

  * ability to specify expected outputs in ghci blocks

  * prompt for password if not provided

  * bump `HaXml` upper bound to allow 1.23.*

0.4: 2 July 2012
----------------

  * Add special support for wordpress.com's LaTeX format

  * Support for `[ghci]` blocks with contents automatically passed
    through `ghci` and results typeset

  * Support for tags

  * Support for creating "pages" as well as posts (WordPress only)

  * New standalone documentation

  * Code cleanup

  * Update to build with GHC 7.4.1
