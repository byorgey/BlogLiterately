BlogLiterately is a tool for uploading blog posts to servers that
support the MetaWeblog API (such as WordPress-based blogs and many
others).  It also handles syntax highlighting of Haskell and other
languages.

> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ViewPatterns #-}
> module Main where

> import Text.BlogLiterately.Ghci
> import Text.BlogLiterately.Block
> import Text.BlogLiterately.Highlight

We need [Pandoc][] for parsing [Markdown][]:

> import Text.Pandoc
> import Text.Pandoc.Highlighting             ( highlight, formatHtmlBlock )

And [hscolour][] for highlighting:

> import Language.Haskell.HsColour            ( hscolour, Output(..) )
> import Language.Haskell.HsColour.Colourise  ( defaultColourPrefs )

To post to a blog, we need the [MetaWeblog][] API, which is an
XML-RPC-based protocol for interacting with blogs.

We'll use the Haskell XML-RPC library, [HaXR][], by Bjorn Bringert,
(on [hackage][hackage-haxr]).

> import Network.XmlRpc.Client                ( remote )
> import Network.XmlRpc.Internals             ( Value(..), toValue )

We use Neil Mitchell's [CmdArgs][] library for processing command-line
arguments:

> import System.Console.CmdArgs

We also need to parse and manipulate XHTML, so we'll use Malcolm
Wallace's [HaXml][] XML combinators, and blaze-html for rendering
HTML:

> import Text.XML.HaXml
> import Text.XML.HaXml.Posn                  ( noPos )
> import Text.Blaze.Html.Renderer.String      ( renderHtml )

Finally, some miscellaneous/standard imports:

> import           Control.Arrow              ( first, (>>>), arr
>                                             , Kleisli(..), runKleisli )
> import qualified Control.Category as C      ( Category, id )
> import           Control.Monad              ( liftM, unless )
> import           Control.Monad.IO.Class     ( liftIO )
> import           Control.Monad.Trans.Reader ( ReaderT, runReaderT, ask )
> import qualified Data.ByteString.Char8 as B
> import           Data.Char                  ( toLower )
> import           Data.Functor               ( (<$>) )
> import           Data.List                  ( isPrefixOf, intercalate )
> import           System.FilePath            ( takeFileName, takeExtension )
> import           System.IO
> import qualified System.IO.UTF8 as U        ( readFile )
> import           System.Process             ( ProcessHandle, waitForProcess
>                                             , runInteractiveCommand )
> import           Text.ParserCombinators.Parsec


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

Finally, a function to upload embedded images from the post to the
server.

> uploadAllImages :: BlogLiterately -> (Pandoc -> IO Pandoc)
> uploadAllImages bl@(BlogLiterately{..}) =
>   case (blog, uploadImages) of
>     (Just xmlrpc, True) -> bottomUpM (uploadOneImage xmlrpc)
>     _                   -> return
>   where
>     uploadOneImage :: String -> Inline -> IO Inline
>     uploadOneImage xmlrpc i@(Image altText (imgUrl, imgTitle))
>       | isLocal imgUrl = do
>           res <- uploadIt xmlrpc imgUrl bl
>           case res of 
>             ValueStruct (lookup "url" -> Just (ValueString newUrl)) ->
>               return $ Image altText (newUrl, imgTitle)
>             _ -> do
>               putStrLn $ "Warning: upload of " ++ imgUrl ++ " failed."
>               return i
>       | otherwise      = return i
>     uploadOneImage _ i = return i
>
>     isLocal imgUrl = none (`isPrefixOf` imgUrl) ["http", "/"]
>     none p = all (not . p)
>
> uploadIt :: String -> FilePath -> BlogLiterately -> IO Value
> uploadIt url filePath (BlogLiterately{..}) = do
>   putStrLn $ "Uploading " ++ filePath ++ "..."
>   media <- mkMediaObject filePath
>   remote url "metaWeblog.newMediaObject" blogid user password media
>
> mkMediaObject :: FilePath -> IO Value
> mkMediaObject filePath = do
>   bits <- B.unpack <$> B.readFile filePath
>   return $ ValueStruct
>     [ ("name", toValue fileName)
>     , ("type", toValue fileType)
>     , ("bits", ValueBase64 bits)
>     ]
>   where
>     fileName = takeFileName filePath
>     fileType = case (map toLower . drop 1 . takeExtension) fileName of
>                  "png"  -> "image/png"
>                  "jpg"  -> "image/jpeg"
>                  "jpeg" -> "image/jpeg"
>                  "gif"  -> "image/gif"

A useful arrow utility, for running some part of a pipeline
conditionally:

> whenA :: C.Category (~>) => (a ~> a) -> Bool -> (a ~> a)
> whenA a p | p         = a
>           | otherwise = C.id

Finally, putting everything together to transform a complete input
document string to an HTML output string.  Note this may involve
running `ghci`.

> xformDoc :: BlogLiterately -> (String -> IO String)
> xformDoc bl@(BlogLiterately {..}) = runKleisli $
>         arr     fixLineEndings
>     >>> arr     (readMarkdown parseOpts) -- from Pandoc
>     >>> arr     wpTeXify                `whenA` wplatex
>     >>> Kleisli (formatInlineGhci file) `whenA` ghci
>     >>> Kleisli (uploadAllImages bl)
>     >>> arr     (colourisePandoc hsHighlight otherHighlight)
>     >>> arr     (writeHtml writeOpts) -- from Pandoc
>     >>> arr     renderHtml
>   where
>     writeOpts = defaultWriterOptions
>                 { writerReferenceLinks = True }
>     parseOpts = defaultParserState
>                 { stateLiterateHaskell = True }
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
>   , uploadImages   :: Bool          -- automatically upload images?
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
>      , uploadImages = def &= name "upload-images" &= explicit &= help "upload local images"
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
