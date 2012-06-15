This new version of BlogLiterately adds a few more options and tries to allow
the user to take advantage of the Pandoc syntax highlighting, or suppress
it.

> {-# LANGUAGE DeriveDataTypeable #-}
> module Main where

We need [Pandoc][] for parsing [Markdown][]:

> import Text.Pandoc
> import Text.Pandoc.Highlighting

And [hscolour][] for highlighting:

> import Language.Haskell.HsColour(hscolour,Output(..))
> import Language.Haskell.HsColour.Colourise(defaultColourPrefs)

To post to a blog, we need the [MetaWeblog][] API, which is an XML-RPC-based
protocol for interacting with blogs.

We'll use the Haskell XML-RPC library, [HaXR][], by Bjorn Bringert, (on 
[hackage][hackage-haxr]). *Note: the latest version (as of this writing) of 
HaXR on Hackage does not specify an upper bound in its dependency on HaXml, but
it is incompatible with the 1.19 versions of HaXml!  If you have HaXml-1.19.* 
installed, you'll have to work around this.*

> import Network.XmlRpc.Client
> import Network.XmlRpc.Internals

And it works that out I'll need some miscellaneous other stuff.  Since I'm 
writing a command line tool, I'll need to process the command line arguments, 
and Neil Mitchell's [CmdArgs][] library ought to work for that:

> import System.Console.CmdArgs

I'm going to end up needing to parse and manipulate XHTML, so I'll use Malcolm
Wallace's [HaXml][] XML combinators:

> import Text.XML.HaXml
> import Text.XML.HaXml.Posn
> import Text.XML.HaXml.Verbatim

> import qualified System.IO.UTF8 as U

> import Control.Monad(liftM,unless)
> import Text.XHtml.Transitional(showHtmlFragment)
> import Text.ParserCombinators.Parsec

The program will read in a literate Haskell file, use Pandoc to parse it as 
markdown, and, if it is using hscolour to for the Haskell pieces, will use
hscolour to transform those.  Pandoc turns its input into a structure of type:

    [haskell]
    data Pandoc = Pandoc Meta [Block]
    
where a `Block` (the interesting bit, for my purposes) looks like:

    [haskell]
    -- | Block element.
    data Block  
        = Plain [Inline]        -- ^ Plain text, not a paragraph
        | Para [Inline]         -- ^ Paragraph
        | CodeBlock Attr String -- ^ Code block (literal) with attributes 
        | RawHtml String        -- ^ Raw HTML block (literal)
        | BlockQuote [Block]    -- ^ Block quote (list of blocks)
        | OrderedList ListAttributes [[Block]] -- ^ Ordered list (attributes
                                -- and a list of items, each a list of blocks)
        | BulletList [[Block]]  -- ^ Bullet list (list of items, each
                                -- a list of blocks)
        | DefinitionList [([Inline],[Block])]  -- ^ Definition list 
                                -- (list of items, each a pair of an inline list,
                                -- the term, and a block list)
        | Header Int [Inline]   -- ^ Header - level (integer) and text (inlines) 
        | HorizontalRule        -- ^ Horizontal rule
        | Table [Inline] [Alignment] [Double] [[Block]] [[[Block]]]  -- ^ Table,
                                -- with caption, column alignments,
                                -- relative column widths, column headers
                                -- (each a list of blocks), and rows
                                -- (each a list of lists of blocks)
        | Null                  -- ^ Nothing
        deriving (Eq, Read, Show, Typeable, Data)

The literate Haskell that Pandoc finds in a file ends up in various `CodeBlock`
elements of the `Pandoc` document.  Other code can also wind up in `CodeBlock`
elements -- normal markdown formatted code.  The `Attr` component has 
metadata about what's in the code block:

    [haskell]
    type Attr = (String, -- code block identifier
                     [String], -- list of code classes
                     [(String, String)]) -- name/value pairs

Thanks to some feedback from the Pandoc author, John MacFarlane, I learned that
the CodeBlock *may* contain markers about the kind of code contained within the
block.  LHS (bird-style or LaTex style) will always have an `Attr` of the form
`("",["sourceCode","haskell"],[])`, and other `CodeBlock`
elements are the markdown code blocks *may* have an identifier, classes, or 
key/value pairs.  Pandoc captures this info when the file contains code blocks
in the delimited (rather than indented) format, which allows an optional 
meta-data specification, e.g.

~~~~~~~~~~~
~~~~~~~ { .bash }
x=$1
echo $x
~~~~~~~
~~~~~~~~~~~

Although Pandoc supports the above format for marking code blocks (and 
annotating the kind of code within the block) I'll also keep my notation as
another option for use with indented blocks, i.e. if you write:

<pre><code>
    [haskell]
    foo :: String -> String
</code></pre>

it is a Haskell block.  If it looks like something else, e.g.

<pre><code>
    [cpp]
    cout << "Hello World!";
</code></pre>

or
<pre><code>
    [other]
    foo bar baz
</pre></code>

If highlighting-kate is specified for highlighting Haskell blocks, the distinction
between the literate blocks and the delimited blocks is lost (this is simply how
the Pandoc highlighting module currently works).

I'll adopt the rule that if you specify a class or
classes using Pandoc's delimited code block syntax, I'll assume that there is 
no additional tag within the block in Blog Literately syntax.  I still need my
`unTag` function to parse the code block.

> unTag :: String -> (String, String)
> unTag s = either (const ("",s)) id $ parse tag "" s
>    where tag = do
>              tg <- between (char '[') (char ']') $ many $ noneOf "[]"
>              skipMany $ oneOf " \t"
>              (string "\r\n" <|> string "\n")
>              txt <- many $ anyToken
>              eof
>              return (tg,txt)

To highlight the syntax using hscolour (which produces HTML), I'm going to
need to transform the `String` from a `CodeBlock` element to a `String` 
suitable for the `RawHtml` element (because the hscolour library transforms
Haskell text to HTML). Pandoc strips off the prepended &gt; characters from the
literate Haskell, so I need to put them back, and also tell hscolour whether the
source it is colouring is literate or not.  The hscolour function looks like:

    [haskell]
    hscolour :: Output      -- ^ Output format.
             -> ColourPrefs -- ^ Colour preferences...
             -> Bool        -- ^ Whether to include anchors.
             -> Bool        -- ^ Whether output document is partial or complete.
             -> String      -- ^ Title for output.
             -> Bool        -- ^ Whether input document is literate haskell
             -> String      -- ^ Haskell source code.
             -> String      -- ^ Coloured Haskell source code.

Since I still don't like the `ICSS` output from hscolour, I'm going to provide
two options for hscolouring to users: one that simply uses hscolour's `CSS`
format, so the user can provide definitions in their blog's stylesheet to
control the rendering, and a post-processing option to transform the `CSS`
class-based rendering into a inline style based rendering (for people who can't
update their stylesheet).  `colourIt` performs the initial transformation:

> colourIt literate srcTxt = 
>     hscolour CSS defaultColourPrefs False True "" literate srcTxt'
>     where srcTxt' | literate = prepend srcTxt
>                   | otherwise = srcTxt
    
Prepending the literate Haskell markers on the source:

> prepend s = unlines $ map ("> " ++) $ lines s

Hscolour uses HTML `span` elements and CSS classes like 'hs-keyword' or 
`hs-keyglyph` to markup Haskell code.  What I want to do is take each marked 
`span` element and replace the `class` attribute with an inline `style` element
that has the markup I want for that kind of source.  I've rethought the style 
preferences type, and think it will be simpler, and more general, as just a list
of name/value pairs:

> type StylePrefs = [(String,String)]

The default style that produces something like what the source listings
on Hackage look like is now:

> defaultStylePrefs = [
>     ("hs-keyword","color: blue; font-weight: bold;")
>   , ("hs-keyglyph","color: red;")
>   , ("hs-layout","color: red;")
>   , ("hs-comment","color: green;")
>   , ("hs-conid", "")
>   , ("hs-varid", "")
>   , ("hs-conop", "")
>   , ("hs-varop", "")
>   , ("hs-str", "color: teal;")
>   , ("hs-chr", "color: teal;")
>   , ("hs-number", "")
>   , ("hs-cpp", "")
>   , ("hs-selection", "")
>   , ("hs-variantselection", "")
>   , ("hs-definition", "")]

I can read these preferences in from a file using the `Read` instance for
`StylePrefs`.  I could handle errors better, but this should work:

> getStylePrefs "" = return defaultStylePrefs
> getStylePrefs fname = liftM read (U.readFile fname)

Hscolour produces a `String` of HTML.  To 'bake' the styles into
the HTML it, we need to parse it, manipulate it
and then re-render it as a `String`.  Use HaXml to do all of this:

> bakeStyles :: StylePrefs -> String -> String
> bakeStyles prefs s =  verbatim $ filtDoc (xmlParse "bake-input" s) where
>     -- filter the document (an Hscoloured fragment of Haskell source)
>     filtDoc (Document p s e m) =  c where
>         [c] = filts (CElem e noPos)
>     -- the filter is a fold of individual filters for each CSS class
>     filts = mkElem "pre" [(foldXml $ foldl o keep $ map filt prefs) `o` replaceTag "code"]
>     -- an individual filter replaces the attributes of a tag with
>     -- a style attribute when it has a specific 'class' attribute.
>     filt (cls,style) =
>         replaceAttrs [("style",style)] `when`
>             (attrval $ ("class",AttValue [Left cls]))

Highlighting-Kate uses &lt;br/> in code blocks to indicate newlines.  WordPress
(if not other software) chooses to strip them away when found in &lt;pre> sections
of uploaded HTML.  So need to turn them back to newlines.

> replaceBreaks :: String -> String
> replaceBreaks s = verbatim $ filtDoc (xmlParse "input" s) where
>    -- filter the document (a highlighting-kate hitlited fragment of
>    -- haskell source
>    filtDoc (Document p s e m) = c where
>        [c] = filts (CElem e noPos)
>    filts = foldXml (literal "\n" `when` tag "br")

Note to self: the above is a function that could be made better in a 
few ways and then factored out into a library.  A way to handle the 
above would be to allow the preferences to be specified as an actual CSS
style sheet, which then would be baked into the HTML.  Such a function
could be separately useful, and could be used to 'bake' in the
highlighting-kate styles.

To completely colourise/highlight a `CodeBlock` we now can create a function
that transforms a `CodeBlock` into a `RawHtml` block, where the content contains
marked up Haskell (possibly with literate markers), or marked up non-Haskell, if
highlighting of non-Haskell has been selected.

> colouriseCodeBlock :: HsHighlight -> Bool -> Block -> Block
> colouriseCodeBlock hsHilite otherHilite b@(CodeBlock attr@(_,classes,_) s) =
>     if tag == "haskell" || haskell
>         then case hsHilite of
>             HsColourInline style -> 
>                 RawHtml $ bakeStyles style $ colourIt lit src
>             HsColourCSS -> RawHtml $ colourIt lit src
>             HsNoHighlight -> RawHtml $ simpleHTML hsrc
>             HsKate -> if null tag 
>                 then myHiliteK attr hsrc
>                 else myHiliteK ("",tag:classes,[]) hsrc
>         else if otherHilite
>             then case tag of
>                 "" -> myHiliteK attr src
>                 t -> myHiliteK ("",[t],[]) src
>             else RawHtml $ simpleHTML src     
>     where (tag,src) = if null classes then unTag s else ("",s)
>           hsrc = if lit then prepend src else src
>           lit = "sourceCode" `elem` classes
>           haskell = "haskell" `elem` classes
>           simpleHTML s = "<pre><code>" ++ s ++ "</code></pre>"
>           myHiliteK attr s = case highlightHtml attr s of
>               Left _ -> RawHtml $ simpleHTML s
>               Right html -> RawHtml $ replaceBreaks $ showHtmlFragment html
> colouriseCodeBlock _ _ b = b

Colourising a `Pandoc` document is simply:

> colourisePandoc hsHilite otherHilite (Pandoc m blocks) = 
>     Pandoc m $ map (colouriseCodeBlock hsHilite otherHilite) blocks

Transforming a complete input document string to an HTML output string:

> xformDoc :: HsHighlight -> Bool -> String -> String
> xformDoc hsHilite otherHilite s = 
>     showHtmlFragment 
>     $ writeHtml writeOpts -- from Pandoc
>     $ colourisePandoc hsHilite otherHilite
>     $ readMarkdown parseOpts -- from Pandoc
>     $ fixLineEndings s
>     where writeOpts = defaultWriterOptions {
>               --writerLiterateHaskell = True,
>               writerReferenceLinks = True }
>           parseOpts = defaultParserState { 
>               stateLiterateHaskell = True }
>           -- readMarkdown is picky about line endings
>           fixLineEndings [] = []
>           fixLineEndings ('\r':'\n':cs) = '\n':fixLineEndings cs
>           fixLineEndings (c:cs) = c:fixLineEndings cs


The metaWeblog API defines a `newPost` and  `editPost` procedures that look
like:

    [other]
    metaWeblog.newPost (blogid, username, password, struct, publish)
        returns string
    metaWeblog.editPost (postid, username, password, struct, publish)
        returns true

For my blog (a WordPress blog), the `blogid` is just `default`.  The user
name and password are simply strings, and `publish` is a flag indicating whether
to load the post as a draft, or to make it public immediately.  The `postid` is
an identifier string which is assigned when you initially create a post. The
interesting bit is the `struct` field, which is an XML-RPC structure defining 
the post along with some meta-data, like the title.  I want be able to provide
the post body, a title, and a list of categories.  The for the
body and title, we could just let HaXR convert the values automatically
into the XML-RPC `Value` type, since they all have the same Haskell type
(`String`) and thus can be put into a list.  But the categories are a list of
strings, so we need to explicitly convert everything to a `Value`, then combine:

> mkPost title text categories = 
>     cats ++ [("title",toValue title),("description",toValue text)]
>     where cats = if null categories then [] 
>               else [("categories",toValue categories)]

The HaXR library exports a function for invoking XML-RPC procedures:

    [haskell]
    remote :: Remote a => 
        String -- ^ Server URL. May contain username and password on
               --   the format username:password\@ before the hostname.
           -> String -- ^ Remote method name.
           -> a      -- ^ Any function 
         -- @(XmlRpcType t1, ..., XmlRpcType tn, XmlRpcType r) => 
                     -- t1 -> ... -> tn -> IO r@

The function requires an URL and a method name, and returns a function of type
`Remote a => a`.  Based on the instances defined for `Remote`, any function
with zero or more parameters in the class `XmlRpcType` and a return type of
`XmlRpcType r => IO r` will work, which means you can simply 'feed' `remote`
additional arguments as required by the remote procedure, and as long as you
make the call in an IO context, it will typecheck.  So to call the
`metaWeblog.newPost` procedure, I can do something like:

> postIt :: String -> String -> String -> String -> String -> String 
>     -> [String] -> Bool -> IO String
> postIt url blogId user password title text cats publish =
>     remote url "metaWeblog.newPost" blogId user password 
>         (mkPost title text cats) publish

To update (replace) a post, the function would be:

> updateIt :: String -> String -> String -> String -> String -> String 
>     -> [String] -> Bool -> IO Bool
> updateIt url postId user password title text cats publish =
>     remote url "metaWeblog.editPost" postId user password
>         (mkPost title text cats) publish

There are four modes of Haskell highlighting:

> data HsHighlight = HsColourInline { hsStylePrefs :: StylePrefs }
>     | HsColourCSS | HsKate | HsNoHighlight
>     deriving (Data,Typeable,Show,Eq)

And two modes for other code (off or on!).

We can figure out if Pandoc is linked with highlighting-kate (we
won't show the kate-related options if it isn't):

> noKate = null defaultHighlightingCss

To create a command line program,  I can capture the command line controls in a type:

> data BlogLiterately = BlogLiterately {
>        test :: Bool,       -- do a dry-run: html goes to stdout
>        style :: String,    -- name of a style file
>        hshighlight :: HsHighlight,
>        highlightOther :: Bool, -- use highlight-kate to highlight other code
>        publish :: Bool,    -- an indication of whether the post should be
>                                -- published, or loaded as a draft
>        categories :: [String], --
>        blogid :: String,   -- blog-specific identifier (e.g. for blogging
>                                -- software handling multiple blogs)
>        blog :: String,     -- blog xmlrpc URL
>        user :: String,     -- blog user name
>        password :: String, -- blog password
>        title :: String,    -- post title
>        file :: String,     -- file to post
>        postid :: String    -- id of a post to updated
>     } deriving (Show,Data,Typeable)

And using CmdArgs, this bit of impure evil defines how the command line arguments
work:

> bl = mode $ BlogLiterately {
>     test = def &= text "do a test-run: html goes to stdout, is not posted",
>     style = "" &= text "Style Specification (for --hscolour-icss)" & typFile,
>     hshighlight = enum (HsColourInline defaultStylePrefs)
>         ([ (HsColourInline defaultStylePrefs) &= explicit & 
>                flag "hscolour-icss" & text inline,
>            HsColourCSS &= explicit & flag "hscolour-css" & text css,
>            HsNoHighlight &= explicit & flag "hs-nohilight" &
>                text "no haskell hilighting" ] ++
>           (if noKate then []  else
>                [HsKate &= explicit & flag "hs-kate" & text hskate])),
>     highlightOther = enum False 
>         (if noKate then [] else 
>              [True &= explicit & flag "other-code-kate" &
>               text "hilight other code with highlighting-kate"]),
>     publish = def &= text "Publish post (otherwise it's uploaded as a draft)",
>     categories = def &= explicit & flag "category" & 
>         text "post category (can specify more than one)",
>     blogid = "default" &= text "Blog specific identifier",
>     blog = def &= argPos 0 & typ "URL" 
>         & text "URL of blog's xmlrpc address (e.g. http://example.com/blog/xmlrpc.php)",
>     user = def &= argPos 1 & typ "USER" & text "blog author's user name" ,
>     password = def &= argPos 2 & typ "PASSWORD" & text "blog author's password",
>     title = def &= argPos 3 & typ "TITLE",
>     file = def &=  argPos 4 & typ "FILE" & text "literate haskell file",
>     postid = "" &= text "Post to replace (if any)" } where
>     inline =  "hilight haskell: hscolour, inline style (default)"
>     css = "hilight haskell: hscolour, separate stylesheet"
>     hskate = "hilight haskell with highlighting-kate"

The main blogging function uses the information captured in the `BlogLiterately`
type to read the style preferences, read the input file and transform it, and
post it to the blog:

> blogLiterately (BlogLiterately test style hsmode other pub cats blogid url
>         user pw title file postid) = do
>     prefs <- getStylePrefs style
>     let hsmode' = case hsmode of
>             HsColourInline _ -> HsColourInline prefs
>             _ -> hsmode
>     html <- liftM (xformDoc hsmode' other) $ U.readFile file
>     if test
>        then putStr html
>        else if null postid 
>            then do
>                postid <- postIt url blogid user pw title html cats pub
>                putStrLn $ "post Id: " ++ postid
>            else do
>                result <- updateIt url postid user pw title html cats pub
>                unless result $ putStrLn "update failed!"

And the main program is simply:

> main = cmdArgs info [bl] >>= blogLiterately
>    where info = "BlogLierately v0.3, (C) Robert Greayer 2010\n" ++
>                 "This program comes with ABSOLUTELY NO WARRANTY\n"

I can run it to get some help:

[markdown]: http://daringfireball.net/projects/markdown/
[pandoc]: http://johnmacfarlane.net/pandoc/ "Pandoc"
[hackage]: http://hackage.haskell.org/packages/hackage.html
[haddock]: http://www.haskell.org/haddock/
[hscolour]: http://www.cs.york.ac.uk/fp/darcs/hscolour/
[metaweblog]: http://www.xmlrpc.com/metaWeblogApi
[haxr]: http://www.haskell.org/haxr/
[hackage-haxr]: http://hackage.haskell.org/package/haxr
[cmdargs]: http://community.haskell.org/~ndm/cmdargs/
[haxml]: http://www.cs.york.ac.uk/fp/HaXml/
