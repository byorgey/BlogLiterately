BlogLiterately is a tool for uploading blog posts to servers that
support the MetaWeblog API (such as WordPress-based blogs and many
others).  It also handles syntax highlighting of Haskell and other
languages.

> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE RecordWildCards #-}
> module Main where

We need [Pandoc][] for parsing [Markdown][]:

> import Text.Pandoc
> import Text.Pandoc.Highlighting

And [hscolour][] for highlighting:

> import Language.Haskell.HsColour           (hscolour, Output(..))
> import Language.Haskell.HsColour.Colourise (defaultColourPrefs)

To post to a blog, we need the [MetaWeblog][] API, which is an
XML-RPC-based protocol for interacting with blogs.

We'll use the Haskell XML-RPC library, [HaXR][], by Bjorn Bringert,
(on [hackage][hackage-haxr]).

> import Network.XmlRpc.Client
> import Network.XmlRpc.Internals

We use Neil Mitchell's [CmdArgs][] library for processing command-line
arguments:

> import System.Console.CmdArgs

We also need to parse and manipulate XHTML, so we'll use Malcolm
Wallace's [HaXml][] XML combinators, and blaze-html for rendering
HTML:

> import Text.XML.HaXml
> import Text.XML.HaXml.Posn
> import Text.Blaze.Html.Renderer.String

Finally, some miscellaneous/standard imports:

> import           Control.Arrow              ( first, (>>>), arr
>                                             , Kleisli(..), runKleisli )
> import qualified Control.Category as A      (id)
> import           Control.Monad              (liftM,unless)
> import           Control.Monad.IO.Class     (liftIO)
> import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
> import           Data.Functor
> import           Data.List                  (isPrefixOf, intercalate)
> import           System.IO
> import qualified System.IO.UTF8 as U
> import           System.Process
> import           Text.ParserCombinators.Parsec

The program will read in a literate Haskell file, use Pandoc to parse
it as markdown, and, if it is using hscolour to for the Haskell
pieces, hscolour to transform those.  Pandoc turns its input into a
structure of type:

    [haskell]
    data Pandoc = Pandoc Meta [Block]
 
where a `Block` (the interesting bit, for our purposes) looks like:

    [haskell]
    -- | Block element.
    data Block
        = Plain [Inline]        -- ^ Plain text, not a paragraph
        | Para [Inline]         -- ^ Paragraph
        | CodeBlock Attr String -- ^ Code block (literal) with attributes
        | RawBlock Format String -- ^ Raw block
        | BlockQuote [Block]    -- ^ Block quote (list of blocks)
        | OrderedList ListAttributes [[Block]] -- ^ Ordered list (attributes
                                -- and a list of items, each a list of blocks)
        | BulletList [[Block]]  -- ^ Bullet list (list of items, each
                                -- a list of blocks)
        | DefinitionList [([Inline],[[Block]])]  -- ^ Definition list
                                -- Each list item is a pair consisting of a
                                -- term (a list of inlines) and one or more
                                -- definitions (each a list of blocks)
        | Header Int [Inline]   -- ^ Header - level (integer) and text (inlines)
        | HorizontalRule        -- ^ Horizontal rule
        | Table [Inline] [Alignment] [Double] [TableCell] [[TableCell]]  -- ^ Table,
                                -- with caption, column alignments,
                                -- relative column widths (0 = default),
                                -- column headers (each a list of blocks), and
                                -- rows (each a list of lists of blocks)
        | Null                  -- ^ Nothing
        deriving (Eq, Ord, Read, Show, Typeable, Data GENERIC)

The literate Haskell that Pandoc finds in a file ends up in various
`CodeBlock` elements of the `Pandoc` document.  Other code can also
wind up in `CodeBlock` elements -- normal markdown formatted code.
The `Attr` component has metadata about what's in the code block:

    [haskell]
    type Attr = ( String,             -- code block identifier
                , [String]            -- list of code classes
                , [(String, String)]  -- name/value pairs
                )

Thanks to some feedback from the Pandoc author, John MacFarlane, I
learned that the CodeBlock *may* contain markers about the kind of
code contained within the block.  LHS (bird-style or LaTex style) will
always have an `Attr` of the form `("",["sourceCode","haskell"],[])`,
and other `CodeBlock` elements are the markdown code blocks *may* have
an identifier, classes, or key/value pairs.  Pandoc captures this info
when the file contains code blocks in the delimited (rather than
indented) format, which allows an optional meta-data specification,
e.g.

~~~~~~~~~~~
~~~~~~~ { .bash }
x=$1
echo $x
~~~~~~~
~~~~~~~~~~~

Although Pandoc supports the above format for marking code blocks (and
annotating the kind of code within the block) I'll also keep my
notation as another option for use with indented blocks, i.e. if you
write:

<pre><code>
    [haskell]
    foo :: String -> String
</code></pre>

it is a Haskell block.  You can also use other annotations, *e.g.*

<pre><code>
    [cpp]
    cout << "Hello World!";
</code></pre>

If highlighting-kate is specified for highlighting Haskell blocks, the
distinction between the literate blocks and the delimited blocks is
lost (this is simply how the Pandoc highlighting module currently
works).

I'll adopt the rule that if you specify a class or classes using
Pandoc's delimited code block syntax, I'll assume that there is no
additional tag within the block in Blog Literately syntax.  I still
need my `unTag` function to parse the code block.

> unTag :: String -> (String, String)
> unTag s = either (const ("",s)) id $ parse tag "" s
>   where
>     tag = do
>       tg <- between (char '[') (char ']') $ many $ noneOf "[]"
>       skipMany $ oneOf " \t"
>       (string "\r\n" <|> string "\n")
>       txt <- many $ anyToken
>       eof
>       return (tg,txt)

To highlight the syntax using hscolour (which produces HTML), I'm
going to need to transform the `String` from a `CodeBlock` element to
a `String` suitable for the `RawHtml` element (because the hscolour
library transforms Haskell text to HTML). Pandoc strips off the
prepended &gt; characters from the literate Haskell, so I need to put
them back, and also tell hscolour whether the source it is colouring
is literate or not.  The hscolour function looks like:

    [haskell]
    hscolour :: Output      -- ^ Output format.
             -> ColourPrefs -- ^ Colour preferences...
             -> Bool        -- ^ Whether to include anchors.
             -> Bool        -- ^ Whether output document is partial or complete.
             -> String      -- ^ Title for output.
             -> Bool        -- ^ Whether input document is literate haskell
             -> String      -- ^ Haskell source code.
             -> String      -- ^ Coloured Haskell source code.

Since I still don't like the `ICSS` output from hscolour, I'm going to
provide two options for hscolouring to users: one that simply uses
hscolour's `CSS` format, so the user can provide definitions in their
blog's stylesheet to control the rendering, and a post-processing
option to transform the `CSS` class-based rendering into a inline
style based rendering (for people who can't update their stylesheet).
`colourIt` performs the initial transformation:

> colourIt literate srcTxt =
>     hscolour CSS defaultColourPrefs False True "" literate srcTxt'
>     where srcTxt' | literate = prepend srcTxt
>                   | otherwise = srcTxt

Prepending the literate Haskell markers on the source:

> prepend = unlines . map ("> " ++) . lines

Hscolour uses HTML `span` elements and CSS classes like 'hs-keyword'
or `hs-keyglyph` to markup Haskell code.  What I want to do is take
each marked `span` element and replace the `class` attribute with an
inline `style` element that has the markup I want for that kind of
source.  Style preferences are specified as a list of name/value
pairs:

> type StylePrefs = [(String,String)]

Here's a default style that produces something like what the source
listings on Hackage look like:

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

I can read these preferences in from a file using the `Read` instance
for `StylePrefs`.  I could handle errors better, but this should work:

> getStylePrefs ""    = return defaultStylePrefs
> getStylePrefs fname = liftM read (U.readFile fname)

Hscolour produces a `String` of HTML.  To 'bake' the styles into the
HTML, we need to parse it, manipulate it and then re-render it as a
`String`.  We use HaXml to do all of this:

> bakeStyles :: StylePrefs -> String -> String
> bakeStyles prefs s = verbatim $ filtDoc (xmlParse "bake-input" s)
>   where
>
>     -- filter the document (an Hscoloured fragment of Haskell source)
>     filtDoc (Document p s e m) =  c where
>         [c] = filts (CElem e noPos)
>
>     -- the filter is a fold of individual filters for each CSS class
>     filts = mkElem "pre" [(foldXml $ foldl o keep $ map filt prefs) `o` replaceTag "code"]
>
>     -- an individual filter replaces the attributes of a tag with
>     -- a style attribute when it has a specific 'class' attribute.
>     filt (cls,style) =
>         replaceAttrs [("style",style)] `when`
>             (attrval $ (N "class", AttValue [Left cls]))

Highlighting-Kate uses &lt;br/> in code blocks to indicate newlines.
WordPress (if not other software) chooses to strip them away when
found in &lt;pre> sections of uploaded HTML.  So we need to turn them
back to newlines.

> replaceBreaks :: String -> String
> replaceBreaks s = verbatim $ filtDoc (xmlParse "input" s)
>   where
>     -- filter the document (a highlighting-kate highlighted fragment of
>     -- haskell source)
>     filtDoc (Document p s e m) = c where
>         [c] = filts (CElem e noPos)
>     filts = foldXml (literal "\n" `when` tag "br")

Note to self: the above is a function that could be made better in a
few ways and then factored out into a library.  A way to handle the
above would be to allow the preferences to be specified as an actual
CSS style sheet, which then would be baked into the HTML.  Such a
function could be separately useful, and could be used to 'bake' in
the highlighting-kate styles.

To completely colourise/highlight a `CodeBlock` we now can create a
function that transforms a `CodeBlock` into a `RawHtml` block, where
the content contains marked up Haskell (possibly with literate
markers), or marked up non-Haskell, if highlighting of non-Haskell has
been selected.

> colouriseCodeBlock :: HsHighlight -> Bool -> Block -> Block
> colouriseCodeBlock hsHighlight otherHighlight b@(CodeBlock attr@(_,classes,_) s)
>
>   | tag == "haskell" || haskell
>   = case hsHighlight of
>         HsColourInline style ->
>             RawBlock "html" $ bakeStyles style $ colourIt lit src
>         HsColourCSS   -> RawBlock "html" $ colourIt lit src
>         HsNoHighlight -> RawBlock "html" $ simpleHTML hsrc
>         HsKate        -> if null tag
>             then myHighlightK attr hsrc
>             else myHighlightK ("",tag:classes,[]) hsrc
>
>   | otherHighlight
>   = case tag of
>         "" -> myHighlightK attr src
>         t  -> myHighlightK ("",[t],[]) src
>
>   | otherwise
>   = RawBlock "html" $ simpleHTML src
>
>   where
>     (tag,src)
>         | null classes = unTag s
>         | otherwise    = ("",s)
>     hsrc
>         | lit          = prepend src
>         | otherwise    = src
>     lit          = "sourceCode" `elem` classes
>     haskell      = "haskell" `elem` classes
>     simpleHTML s = "<pre><code>" ++ s ++ "</code></pre>"
>     myHighlightK attr s = case highlight formatHtmlBlock attr s of
>         Nothing   -> RawBlock "html" $ simpleHTML s
>         Just html -> RawBlock "html" $ replaceBreaks $ renderHtml html
>
> colouriseCodeBlock _ _ b = b

Colourising a `Pandoc` document is simply:

> colourisePandoc hsHighlight otherHighlight (Pandoc m blocks) =
>     Pandoc m $ map (colouriseCodeBlock hsHighlight otherHighlight) blocks

WordPress can render LaTeX, but expects it in a special (non-standard)
format (`\$latex foo\$`).  The `wpTeXify` function formats LaTeX code
using this format so that it can be processed by WordPress.

> wpTeXify :: Pandoc -> Pandoc
> wpTeXify = bottomUp formatDisplayTex . bottomUp formatInlineTex
>   where formatInlineTex :: [Inline] -> [Inline]
>         formatInlineTex (Math InlineMath tex : is)
>           = (Str $ "$latex " ++ unPrefix "latex" tex ++ "$") : is
>         formatInlineTex is = is
>
>         formatDisplayTex :: [Block] -> [Block]
>         formatDisplayTex (Para [Math DisplayMath tex] : bs)
>           = RawBlock "html" "<p><div style=\"text-align: center\">"
>           : Plain [Str $ "$latex " ++ "\\displaystyle " ++ unPrefix "latex" tex ++ "$"]
>           : RawBlock "html" "</div></p>"
>           : bs
>         formatDisplayTex bs = bs
>
>         unPrefix pre s
>           | pre `isPrefixOf` s = drop (length pre) s
>           | otherwise          = s

The next bit of code enables using code blocks marked with `[ghci]` as
input to ghci and then inserting the results.  This code was mostly
stolen from lhs2TeX.

> type ProcessInfo = (Handle, Handle, Handle, ProcessHandle)

First, a way to evaluate an expression using an external ghci process.

> ghciEval :: String -> ReaderT ProcessInfo IO String
> ghciEval expr =  do
>   (pin, pout, _, _) <- ask
>   let script = "putStrLn " ++ show magic ++ "\n"
>                  ++ expr ++ "\n"
>                  ++ "putStrLn " ++ show magic ++ "\n"
>   liftIO $ do
>     hPutStr pin script
>     hFlush pin
>     extract' pout
>
> withGhciProcess :: FilePath -> ReaderT ProcessInfo IO a -> IO a
> withGhciProcess f m = do
>   isLit <- isLiterate f
>   pi    <- runInteractiveCommand $ "ghci -v0 -ignore-dot-ghci "
>                                    ++ (if isLit then f else "")
>   res   <- runReaderT m pi
>   stopProcess pi
>   return res
>
> isLiterate :: FilePath -> IO Bool
> isLiterate f = (any ("> " `isPrefixOf`) . lines) <$> readFile f
>
> stopProcess :: ProcessInfo -> IO ()
> stopProcess (pin,_,_,pid) = do
>   hPutStrLn pin ":q"
>   hFlush pin
>   _ <- waitForProcess pid   -- ignore exit code
>   return ()

To extract the answer from @ghci@'s output we use a simple technique
which should work in most cases: we print the string |magic| before
and after the expression we are interested in. We assume that
everything that appears before the first occurrence of |magic| on the
same line is the prompt, and everything between the first |magic| and
the second |magic| plus prompt is the result we look for.

> magic :: String
> magic =  "!@#$^&*"
>
> extract' :: Handle -> IO String
> extract' h = fmap (extract . unlines) (readMagic 2)
>   where
>     readMagic :: Int -> IO [String]
>     readMagic 0 = return []
>     readMagic n = do
>       l <- hGetLine h
>       let n' | (null . snd . breaks (isPrefixOf magic)) l = n
>              | otherwise                                  = n - 1
>       fmap (l:) (readMagic n')
>
> extract                       :: String -> String
> extract s                     =  v
>     where (t, u)              =  breaks (isPrefixOf magic) s
>           -- t contains everything up to magic, u starts with magic
>           -- |u'                      =  tail (dropWhile (/='\n') u)|
>           pre                 =  reverse . takeWhile (/='\n') . reverse $ t
>           prelength           =  if null pre then 0 else length pre + 1
>           -- pre contains the prefix of magic on the same line
>           u'                  =  drop (length magic + prelength) u
>           -- we drop the magic string, plus the newline, plus the prefix
>           (v, _)              =  breaks (isPrefixOf (pre ++ magic)) u'
>           -- we look for the next occurrence of prefix plus magic
>
> breaks                        :: ([a] -> Bool) -> [a] -> ([a], [a])
> breaks p []                   =  ([], [])
> breaks p as@(a : as')
>     | p as                    =  ([], as)
>     | otherwise               =  first (a:) $ breaks p as'

Finally, a function which takes the path to the `.lhs` source and its
representation as a `Pandoc` document, finds any `[ghci]` blocks in
it, runs them through `ghci`, and formats the results as an
interactive `ghci` session.

> formatInlineGhci :: FilePath -> Pandoc -> IO Pandoc
> formatInlineGhci f = withGhciProcess f . bottomUpM formatInlineGhci'
>   where
>     formatInlineGhci' :: Block -> ReaderT ProcessInfo IO Block
>     formatInlineGhci' b@(CodeBlock attr s)
>       | tag == "ghci" =  do
>           results <- zip inputs <$> mapM ghciEval inputs
>           return $ CodeBlock attr (intercalate "\n" $ map formatGhciResult results)
>
>       | otherwise = return b
>
>       where (tag,src) = unTag s
>             inputs    = lines src
>
>     formatInlineGhci' b = return b
>
>     formatGhciResult (input, output)
>       = "<span style=\"color: gray;\">ghci&gt;</span> " ++ input ++ (unlines . map ("  "++) . lines) output  -- XXX this should be configurable!

Finally, putting everything together to transform a complete input
document string to an HTML output string:

> xformDoc :: FilePath -> HsHighlight -> Bool -> Bool -> Bool -> (String -> IO String)
> xformDoc f hsHighlight otherHighlight wpl ghci = runKleisli $
>         arr     fixLineEndings
>     >>> arr     (readMarkdown parseOpts) -- from Pandoc
>     >>> arr     wpTeXify             `whenA` wpl
>     >>> Kleisli (formatInlineGhci f) `whenA` ghci
>     >>> arr     (colourisePandoc hsHighlight otherHighlight)
>     >>> arr     (writeHtml writeOpts) -- from Pandoc
>     >>> arr     renderHtml
>   where
>     writeOpts = defaultWriterOptions
>                 { writerReferenceLinks = True }
>     parseOpts = defaultParserState
>                 { stateLiterateHaskell = True }
>
>     whenA a p | p         = a
>               | otherwise = A.id
>
>     -- readMarkdown is picky about line endings
>     fixLineEndings [] = []
>     fixLineEndings ('\r':'\n':cs) = '\n':fixLineEndings cs
>     fixLineEndings (c:cs) = c:fixLineEndings cs

The metaWeblog API defines `newPost` and `editPost` procedures that
look like:

    [other]
    metaWeblog.newPost (blogid, username, password, struct, publish)
        returns string
    metaWeblog.editPost (postid, username, password, struct, publish)
        returns true

For WordPress blogs, the `blogid` is ignored.  The user name and
password are simply strings, and `publish` is a flag indicating
whether to load the post as a draft, or to make it public immediately.
The `postid` is an identifier string which is assigned when you
initially create a post. The interesting bit is the `struct` field,
which is an XML-RPC structure defining the post along with some
meta-data, like the title.  I want be able to provide the post body, a
title, and lists of categories and tags.  For the body and title, we
could just let HaXR convert the values automatically into the XML-RPC
`Value` type, since they all have the same Haskell type (`String`) and
thus can be put into a list.  But the categories and tags are lists of
strings, so we need to explicitly convert everything to a `Value`,
then combine:

> mkPost title text categories tags page =
>        mkArray "categories" categories
>     ++ mkArray "mt_keywords" tags
>     ++ [ ("title", toValue title)
>        , ("description", toValue text)
>        ]
>     ++ [ ("post_type", toValue "page") | page ]
>
> mkArray _    []     = []
> mkArray name values = [(name, toValue values)]

The HaXR library exports a function for invoking XML-RPC procedures:

    [haskell]
    remote :: Remote a =>
        String -- ^ Server URL. May contain username and password on
               --   the format username:password\@ before the hostname.
           -> String -- ^ Remote method name.
           -> a      -- ^ Any function
         -- @(XmlRpcType t1, ..., XmlRpcType tn, XmlRpcType r) =>
                     -- t1 -> ... -> tn -> IO r@

The function requires an URL and a method name, and returns a function
of type `Remote a => a`.  Based on the instances defined for `Remote`,
any function with zero or more parameters in the class `XmlRpcType`
and a return type of `XmlRpcType r => IO r` will work, which means you
can simply 'feed' `remote` additional arguments as required by the
remote procedure, and as long as you make the call in an IO context,
it will typecheck.  `postIt` calls `metaWeblog.newPost` or
`metaWeblog.editPost` (or simply prints the HTML to stdout) as
appropriate:

> postIt :: BlogLiterately -> String -> IO ()
> postIt (BlogLiterately{..}) html =
>   case blog of
>     Nothing  -> putStr html
>     Just url ->
>       case postid of
>         Nothing  -> do
>           pid <- remote url "metaWeblog.newPost" blogid user password
>                    (mkPost title html categories tags page) publish
>           putStrLn $ "Post ID: " ++ pid
>         Just pid -> do
>           success <- remote url "metaWeblog.editPost" pid user password
>                        (mkPost title html categories tags page) publish
>           unless success $ putStrLn "update failed!"

There are four modes of Haskell highlighting:

> data HsHighlight =
>       HsColourInline StylePrefs
>     | HsColourCSS
>     | HsKate
>     | HsNoHighlight
>   deriving (Data,Typeable,Show,Eq)

And two modes for other code (off or on!).

To create a command line program, we capture the command line controls
in a type:

> data BlogLiterately = BlogLiterately
>   { style          :: String        -- name of a style file
>   , hsHighlight    :: HsHighlight   -- Haskell highlighting mode
>   , otherHighlight :: Bool          -- use highlighting-kate for non-Haskell?
>   , wplatex        :: Bool          -- format LaTeX for WordPress?
>   , ghci           :: Bool          -- automatically generate ghci sessions?
> --  , uploadImages   :: Bool          -- automatically upload images?
>   , categories     :: [String]      -- categories for the post
>   , tags           :: [String]      -- tags for the post
>   , blogid         :: String        -- blog-specific identifier (e.g. for blogging
>                                     --   software handling multiple blogs)
>   , blog           :: Maybe String  -- blog xmlrpc URL
>   , user           :: String        -- blog user name
>   , password       :: String        -- blog password
>   , title          :: String        -- post title
>   , file           :: String        -- file to post
>   , postid         :: Maybe String  -- id of a post to update
>   , page           :: Bool          -- create a "page" instead of a post
>   , publish        :: Bool          -- Should the post be published, or
>                                     --   loaded as a draft?
>   }
>   deriving (Show,Data,Typeable)

And using CmdArgs, this bit of impure evil defines how the command
line arguments work:

> bl = BlogLiterately
>      { style = ""  &= help "style specification (for --hscolour-icss)"
>                    &= typFile
>      , hsHighlight = enum
>        [ (HsColourInline defaultStylePrefs)
>          &= explicit
>          &= name "hscolour-icss"
>          &= help "highlight haskell: hscolour, inline style (default)"
>        , HsColourCSS
>          &= explicit
>          &= name "hscolour-css"
>          &= help "highlight haskell: hscolour, separate stylesheet"
>        , HsNoHighlight
>          &= explicit
>          &= name "hs-nohighlight"
>          &= help "no haskell highlighting"
>        , HsKate
>          &= explicit
>          &= name "hs-kate"
>          &= help "highlight haskell with highlighting-kate"
>        ]
>      , otherHighlight = enum
>        [ True
>          &= explicit
>          &= name "other-kate"
>          &= help "highlight other code with highlighting-kate"
>        ]
>      , wplatex = def &= help "reformat inline LaTeX the way WordPress expects"
>      , ghci    = def &= help "run [ghci] blocks through ghci and include output"
> --     , uploadImages = def &= name "upload-images" &= explicit &= help "upload local images"
>      , page    = def &= help "create a \"page\" instead of a post (WordPress only)"
>      , publish = def &= help "publish post (otherwise it's uploaded as a draft)"
>      , categories = def
>        &= explicit
>        &= name "category"
>        &= help "post category (can specify more than one)"
>      , tags = def
>        &= explicit
>        &= name "tag"
>        &= help "tag (can specify more than one)"
>
>      , blogid   = "default" &= help "Blog specific identifier" &= typ "ID"
>      , postid   = def &= help "Post to replace (if any)" &= typ "ID"
>
>      , blog     = def &= typ "URL"      &= help "blog XML-RPC url (if omitted, html goes to stdout)"
>      , user     = def &= typ "USER"     &= help "user name"
>      , password = def &= typ "PASSWORD" &= help "password"
>      , title    = def &= typ "TITLE"    &= help "post title"
>      , file     = def &= argPos 0 &= typ "FILE"
>   }
>   &= program "BlogLiterately"
>   &= summary ("BlogLierately v0.4, (c) Robert Greayer 2008-2010, Brent Yorgey 2012\n" ++
>               "This program comes with ABSOLUTELY NO WARRANTY\n")

The main blogging function uses the information captured in the
`BlogLiterately` type to read the style preferences, read the input
file and transform it, and post it to the blog:

> blogLiterately bl@(BlogLiterately {..}) = do
>     prefs <- getStylePrefs style
>     let hsHighlight' = case hsHighlight of
>             HsColourInline _ -> HsColourInline prefs
>             _                -> hsHighlight
>         bl' = bl { hsHighlight = hsHighlight' }
>     html <- xformDoc bl' =<< U.readFile file
>     postIt bl html

And the main program is simply:

> main = cmdArgs bl >>= blogLiterately

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
