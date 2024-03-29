{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.Transform
-- Copyright   :  (c) 2008-2010 Robert Greayer, 2012-2013 Brent Yorgey
-- License     :  GPL (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- Tools for putting together a pipeline transforming the source for a
-- post into a completely formatted HTML document.
--
-----------------------------------------------------------------------------

module Text.BlogLiterately.Transform
    (
      -- * Standard transforms
      -- $standard

      standardTransforms

    , optionsXF
    , profileXF
    , highlightOptsXF
    , passwordXF
    , titleXF
    , rawtexifyXF
    , wptexifyXF
    , ghciXF
    , uploadImagesXF
    , highlightXF
    , centerImagesXF
    , citationsXF

      -- * Link generation
    , specialLinksXF
    , mkSpecialLinksXF
    , standardSpecialLinks
    , luckyLink
    , wikiLink
    , postLink

      -- * Transforms
    , Transform(..), pureTransform, ioTransform, pioTransform, runTransform, runTransforms

      -- * Transforming documents
    , xformDoc

      -- * Utilities
    , fixLineEndings
    ) where

import           Control.Applicative               ((<$>))
import           Control.Arrow                     ((>>>))
import           Control.Lens                      (has, isn't, use, (%=), (&),
                                                    (.=), (.~), (^.), _1, _2,
                                                    _Just)
import           Control.Monad.State
import           Data.Char                         (isDigit, toLower)
import           Data.List                         (intercalate, isInfixOf,
                                                    isPrefixOf)
import           Data.List.Split                   (splitOn)
import qualified Data.Map                          as M
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid                       (mappend, mempty, (<>))
import qualified Data.Set                          as S
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as LT
import           Data.Traversable                  (traverse)
import           Network.HTTP                      (getRequest, getResponseBody,
                                                    simpleHTTP)
import           System.Directory                  (doesFileExist,
                                                    getAppUserDataDirectory)
import           System.Exit                       (exitFailure)
import           System.FilePath                   (takeExtension, (<.>), (</>))
import           System.IO                         (hFlush, stdout)
import           Text.Blaze.Html.Renderer.String   (renderHtml)
import           Text.HTML.TagSoup
import           Text.Pandoc                       hiding (openURL)
import           Text.Pandoc.Citeproc              (processCitations)
import           Text.Pandoc.Error                 (PandocError)
import           Text.Parsec                       (ParseError)

import           Text.BlogLiterately.Block         (onTag)
import           Text.BlogLiterately.Ghci          (formatInlineGhci)
import           Text.BlogLiterately.Highlight     (HsHighlight (HsColourInline),
                                                    colourisePandoc,
                                                    getStylePrefs,
                                                    _HsColourInline)
import           Text.BlogLiterately.Image         (uploadAllImages)
import           Text.BlogLiterately.LaTeX         (rawTeXify, wpTeXify)
import           Text.BlogLiterately.Options
import           Text.BlogLiterately.Options.Parse (readBLOptions)
import           Text.BlogLiterately.Post          (findTitle, getPostURL)

-- | A document transformation consists of two parts: an actual
--   transformation, expressed as a function over Pandoc documents, and
--   a condition specifying whether the transformation should actually
--   be applied.
--
--   The transformation itself takes a 'BlogLiterately' configuration
--   as an argument.  You may of course ignore it if you do not need
--   to know anything about the configuration.  The @--xtra@ (or @-x@)
--   flag is also provided especially as a method of getting
--   information from the command-line to custom extensions. Arguments
--   passed via @-x@ on the command line are available from the 'xtra'
--   field of the 'BlogLiterately' configuration.
--
--   The transformation is then specified as a stateful computation
--   over both a @BlogLiterately@ options record, and a @Pandoc@
--   document.  It may also have effects in the @IO@ monad.
--
--   * If you have a pure function of type @BlogLiterately -> Pandoc
--     -> Pandoc@, you can use the 'pureTransform' function to create a
--     'Transform'.
--
--   * If you have a function of type @BlogLiterately -> Pandoc -> IO
--     Pandoc@, you can use 'ioTransform'.
--
--   * Otherwise you can directly create something of type @StateT
--     (BlogLiterately, Pandoc) IO ()@.
--
--   For examples, see the implementations of the standard transforms
--   below.
data Transform = Transform
                 { getTransform :: StateT (BlogLiterately, Pandoc) PandocIO ()
                   -- ^ A document transformation, which can transform
                   --   both the document and the options and have
                   --   effects in the IO monad.  The options record
                   --   can be transformed because the document itself
                   --   may contain information which affects the options.
                 , xfCond       :: BlogLiterately -> Bool
                   -- ^ A condition under which to run the transformation.
                 }

-- | Construct a transformation from a pure function.
pureTransform :: (BlogLiterately -> Pandoc -> Pandoc)
              -> (BlogLiterately -> Bool) -> Transform
pureTransform transf = Transform (gets fst >>= \bl -> _2 %= transf bl)

-- | Construct a transformation from a function in the @IO@ monad.
ioTransform :: (BlogLiterately -> Pandoc -> IO Pandoc)
            -> (BlogLiterately -> Bool) -> Transform
ioTransform transf = pioTransform (\b p -> liftIO (transf b p))

-- | Construct a transformation from a function in the @PandocIO@ monad.
pioTransform :: (BlogLiterately -> Pandoc -> PandocIO Pandoc)
             -> (BlogLiterately -> Bool) -> Transform
pioTransform transf = Transform $ do
  (bl,p) <- get
  p' <- lift $ transf bl p
  put (bl,p')

-- | Run a 'Transform' (if its condition is met).
runTransform :: Transform -> StateT (BlogLiterately, Pandoc) PandocIO ()
runTransform t = do
  bl <- gets fst
  when (xfCond t bl) $ getTransform t

-- | Run a pipeline of 'Transform's.
runTransforms :: [Transform] -> BlogLiterately -> Pandoc -> PandocIO (BlogLiterately, Pandoc)
runTransforms ts bl p = execStateT (mapM_ runTransform ts) (bl,p)

--------------------------------------------------
-- Standard transforms
--------------------------------------------------

-- $standard
-- These transforms are enabled by default in the standard
-- @BlogLiterately@ executable.

-- | Pass LaTeX (inline or display) through unchanged (if the @rawlatex@ flag is set).
rawtexifyXF :: Transform
rawtexifyXF = pureTransform (const rawTeXify) rawlatex'

-- | Format embedded LaTeX for WordPress (if the @wplatex@ flag is set).
wptexifyXF :: Transform
wptexifyXF = pureTransform (const wpTeXify) wplatex'

-- | Format embedded @ghci@ sessions (if the @ghci@ flag is set).
ghciXF :: Transform
ghciXF = ioTransform (formatInlineGhci . file') ghci'

-- | Upload embedded local images to the server (if the @uploadImages@
--   flag is set).
uploadImagesXF :: Transform
uploadImagesXF = ioTransform uploadAllImages uploadImages'

-- | Perform syntax highlighting on code blocks.
highlightXF :: Transform
highlightXF = pureTransform
  (\bl -> colourisePandoc (hsHighlight' bl) (otherHighlight' bl))
  (const True)

-- | Center any images which occur in a paragraph by themselves.
--   Inline images are not affected.
centerImagesXF :: Transform
centerImagesXF = pureTransform (const centerImages) (const True)

centerImages :: Pandoc -> Pandoc
centerImages = bottomUp centerImage
  where
    centerImage :: [Block] -> [Block]
    centerImage (img@(Para [Image _attr _altText (_imgUrl, _imgTitle)]) : bs) =
        RawBlock (Format "html") "<div style=\"text-align: center;\">"
      : img
      : RawBlock (Format "html") "</div>"
      : bs
    centerImage bs = bs

-- | Replace special links with appropriate URLs.  Currently, the
--   following types of special links are supported:
--
--   [@lucky::<search>@] The first Google result for @<search>@.
--
--   [@wiki::<title>@] The Wikipedia page for @<title>@.  Note that
--   the page is not checked for existence.
--
--   [@hackage::<pkg>@] The Hackage page for @<pkg>@.
--
--   [@github::<user>/<repo>@] The top page for the given repo on github.
--
--   [@github::<user>/<repo>/#<nnn>@] Link to a particular issue.
--
--   [@github::<user>/<repo>/\@<hash>@] Link to a particular commit.
--
--   [@post::nnnn@] Link to the blog post with post ID @nnnn@.  Note
--   that this form of special link is invoked when @nnnn@ consists of
--   all digits, so it only works on blogs which use numerical
--   identifiers for post IDs (as Wordpress does).
--
--   [@post::<search>@] Link to the most recent blog post (among the
--   20 most recent posts) containing @<search>@ in its title.
--
--   For example, a post written in Markdown format containing
--
--   @
--       This is a post about the game of [Go](wiki::Go (game)).
--   @
--
--   will be formatted in HTML as
--
--   @
--       <p>This is a post about the game of <a href="https://en.wikipedia.org/wiki/Go%20(game)">Go</a>.</p>
--   @
--
--   You can also create a Transform with your own special link types,
--   using 'mkSpecialLinksXF', and I am happy to receive pull requests
--   adding new types of standard special links.
specialLinksXF :: Transform
specialLinksXF = mkSpecialLinksXF standardSpecialLinks

-- | The standard special link types included in 'specialLinksXF':
--   'luckyLink', 'wikiLink', 'postLink', 'githubLink', and
--   'hackageLink'.
standardSpecialLinks :: [SpecialLink]
standardSpecialLinks =
  [ luckyLink
  , wikiLink
  , postLink
  , githubLink
  , hackageLink
  ]

-- | A special link consists of two parts:
--
--   * An identifier string.  If the identifier string is @<id>@, this
--   will trigger for links which are of the form @<id>::XXXX@.
--
--   * A URL generation function.  It takes as input the string
--   following the @::@ (the @XXXX@ in the example above), the
--   configuration record, and must output a URL.
--
--   For example,
--
--   @("twitter", \u _ -> return $ "https://twitter.com/" ++ u)@
--
--   is a simple 'SpecialLink' which causes links of the form
--   @twitter::user@ to be replaced by @https://twitter.com/user@.
type SpecialLink = (Text, Text -> BlogLiterately -> IO Text)

-- | Create a transformation which looks for the given special links
--   and replaces them appropriately. You can use this function with
--   your own types of special links.
mkSpecialLinksXF :: [SpecialLink] -> Transform
mkSpecialLinksXF links = ioTransform (specialLinks links) (const True)

-- | Create a document transformation which looks for the given
--   special links and replaces them appropriately.
specialLinks :: [SpecialLink] -> BlogLiterately -> Pandoc -> IO Pandoc
specialLinks links bl = bottomUpM specialLink
  where
    specialLink :: Inline -> IO Inline
    specialLink i@(Link attrs alt (url, title))
      | Just (typ, target) <- getSpecial url
      = mkLink <$> case lookup (T.toLower typ) links of
                     Just mkURL -> mkURL target bl
                     Nothing    -> return target
      where
        mkLink u = Link attrs alt (u, title)

    specialLink i = return i

    getSpecial url
      | "::" `T.isInfixOf` url =
          let (typ:rest) = T.splitOn "::" url
          in  Just (typ, T.intercalate "::" rest)
      | otherwise = Nothing

-- | Turn @lucky::<search>@ into a link to the first Google result for
-- @<search>@.
luckyLink :: SpecialLink
luckyLink = ("lucky", getLucky)
  where
    getLucky :: Text -> BlogLiterately -> IO Text
    getLucky searchTerm _ = do
      results <- openURL $ "http://www.google.com/search?q=" ++ (T.unpack searchTerm)
      let tags   = parseTags results
          anchor = take 1
            . dropWhile (~/= ("<a>" :: String))
            . dropWhile (~/= ("<h3 class='r'>" :: String))
            $ tags
          url = case anchor of
            [t@(TagOpen{})] -> T.pack . takeWhile (/='&') . dropWhile (/='h') . fromAttrib "href" $ t
            _ -> searchTerm
      return url

-- | Get the contents of the given URL in a simple way.
openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

-- | Given @wiki::<title>@, generate a link to the Wikipedia page for
--   @<title>@.  Note that the page is not checked for existence.
wikiLink :: SpecialLink
wikiLink = ("wiki", \target _ -> return $ T.append "https://en.wikipedia.org/wiki/" target)

-- | @postLink@ handles two types of special links.
--
-- [@post::nnnn@] Link to the blog post with post ID @nnnn@.  Note that
-- this form of special link is invoked when @nnnn@ consists of all
-- digits, so it only works on blogs which use numerical identifiers
-- for post IDs (as Wordpress does).
--
-- [@post::<search>@] Link to the most recent blog post (among the
-- 20 most recent posts) containing @<search>@ in its title.
postLink :: SpecialLink
postLink = ("post", getPostLink)
  where
    getPostLink :: Text -> BlogLiterately -> IO Text
    getPostLink target bl =
      (fromMaybe target . fmap T.pack) <$>
        case (T.all isDigit target, bl ^. blog) of
          (_    , Nothing ) -> return Nothing
          (True , Just url) -> getPostURL url (T.unpack target) (user' bl) (password' bl)
          (False, Just url) -> findTitle 20 url (T.unpack target) (user' bl) (password' bl)

          -- If all digits, replace with permalink for that postid
          -- Otherwise, search titles of 20 most recent posts.
          --   Choose most recent that matches.

-- | @githubLink@ handles several types of special links.
--
-- [@github::<user>/<repo>@] links to a repository.
--
-- [@github::<user>/<repo>/#<nnn>@] links to issue #nnn.
--
-- [@github::<user>/<repo>/\@<hash>@] links to the commit with the
-- given hash.
githubLink :: SpecialLink
githubLink = ("github", getGithubLink)
  where
    getGithubLink :: Text -> BlogLiterately -> IO Text
    getGithubLink target bl = return . T.pack $
      case splitOn "/" (T.unpack target) of
        (user : repo : ghTarget) -> github </> user </> repo </> mkTarget ghTarget
        _ -> github </> (T.unpack target)
    github = "https://github.com/"
    mkTarget []                 = ""
    mkTarget (('@': hash) : _)  = "commit" </> hash
    mkTarget (('#': issue) : _) = "issues" </> issue

-- | A target of the form @hackage::<pkg>@ turns into a link to the
--   package @<pkg>@ on Hackage.
hackageLink :: SpecialLink
hackageLink = ("hackage", getHackageLink)
  where
    getHackageLink :: Text -> BlogLiterately -> IO Text
    getHackageLink pkg bl = return $ T.append hackagePrefix pkg
    hackagePrefix = "http://hackage.haskell.org/package/"

-- | Potentially extract a title from the metadata block, and set it
--   in the options record.
titleXF :: Transform
titleXF = Transform extractTitle (const True)
  where
    extractTitle = do
      (Pandoc (Meta m) _) <- gets snd
      case M.lookup "title" m of
        Just (MetaString s) ->
          setTitle (T.unpack s)
        Just (MetaInlines is) ->
          setTitle (intercalate " " [T.unpack s | Str s <- is])
        _ -> return ()

    -- title set explicitly with --title takes precedence.
    setTitle s = _1.title %= (`mplus` Just s)

-- | Extract blocks tagged with @[BLOpts]@ and use their contents as
--   options.
optionsXF :: Transform
optionsXF = Transform optionsXF' (const True)
  where
    optionsXF' = do
      (errs, opts) <- queryWith extractOptions <$> gets snd
      mapM_ (liftIO . print) errs
      _1 %= (<> opts)
      _2 %= bottomUp (concatMap killOptionBlocks)

-- | Take a block and extract from it a list of parse errors and an
--   options record.  If the blog is not tagged with @[BLOpts]@ these
--   will just be empty.
extractOptions :: Block -> ([ParseError], BlogLiterately)
extractOptions = onTag "blopts" (const (readBLOptions . T.unpack)) (const mempty)

-- | Delete any blocks tagged with @[BLOpts]@.
killOptionBlocks :: Block -> [Block]
killOptionBlocks = onTag "blopts" (const (const [])) (:[])

-- | Prompt the user for a password if the @blog@ field is set but no
--   password has been provided.
passwordXF :: Transform
passwordXF = Transform passwordPrompt passwordCond
  where
    passwordCond bl = ((bl ^. blog)     & has   _Just)
                   && ((bl ^. password) & isn't _Just)
    passwordPrompt  = do
      liftIO $ putStr "Password: " >> hFlush stdout
      pwd <- liftIO getLine
      _1 . password .= Just pwd

-- | Read a user-supplied style file and add its contents to the
--   highlighting options.
highlightOptsXF :: Transform
highlightOptsXF = Transform doHighlightOptsXF (const True)
  where
    doHighlightOptsXF = do
      prefs <- (liftIO . getStylePrefs) =<< use (_1 . style)
      (_1 . hsHighlight) %= Just . maybe (HsColourInline prefs)
                                         (_HsColourInline .~ prefs)

-- | Format citations.
citationsXF :: Transform
citationsXF = pioTransform (const processCitations) citations'

-- | Load options from a profile if one is specified.
profileXF :: Transform
profileXF = Transform doProfileXF (const True)
  where
    doProfileXF = do
      bl  <- use _1
      bl' <- liftIO $ loadProfile bl
      _1 .= bl'

-- | Load additional options from a profile specified in the options
--   record.
loadProfile :: BlogLiterately -> IO BlogLiterately
loadProfile bl =
  case bl^.profile of
    Nothing          -> return bl
    Just profileName -> do
      appDir <- getAppUserDataDirectory "BlogLiterately"

      let profileCfg = appDir </> profileName <.> "cfg"
      e <- doesFileExist profileCfg
      case e of
        False -> do
          putStrLn $ profileCfg ++ ": file not found"
          exitFailure
        True  -> do
          (errs, blProfile) <- readBLOptions <$> readFile profileCfg
          mapM_ print errs
          return $ mappend blProfile bl

-- | The standard set of transforms that are run by default (in order
--   from top to bottom):
--
--   * 'optionsXF': extract options specified in @[BLOpts]@ blocks in the file
--
--   * 'profileXF': load the requested profile (if any)
--
--   * 'passwordXF': prompt the user for a password if needed
--
--   * 'titleXF': extract the title from a special title block
--
--   * 'rawtexifyXF': pass LaTeX through unchanged
--
--   * 'wptexifyXF': turn LaTeX into WordPress format if requested
--
--   * 'ghciXF': run and typeset ghci sessions if requested
--
--   * 'uploadImagesXF': upload images if requested
--
--   * 'centerImagesXF': center images occurring in their own paragraph
--
--   * 'specialLinksXF': replace special link types with URLs
--
--   * 'highlightOptsXF': load the requested highlighting style file
--
--   * 'highlightXF': perform syntax highlighting
--
--   * 'citationsXF': process citations
standardTransforms :: [Transform]
standardTransforms =
  [ -- Has to go first, since it may affect later transforms.
    optionsXF

    -- Has to go second, since we may not know which profile to load
    -- until after the optionsXF pass, and loading a profile may
    -- affect later transforms.
  , profileXF

    -- The order of the rest of these probably doesn't matter that
    -- much, except highlightOptsXF should go before highlightXF.
  , passwordXF
  , titleXF
  , rawtexifyXF
  , wptexifyXF
  , ghciXF
  , uploadImagesXF
  , centerImagesXF
  , specialLinksXF
  , highlightOptsXF
  , highlightXF
  , citationsXF
  ]

--------------------------------------------------
-- Transforming documents
--------------------------------------------------

-- | Transform a complete input document string to an HTML output
--   string, given a list of transformation passes.
xformDoc :: BlogLiterately -> [Transform] -> String -> IO (Either PandocError (BlogLiterately, String))
xformDoc bl xforms s = do
  Right tpl <- compileTemplate "" blHtmlTemplate
  runIO .
    (     fixLineEndings
      >>> T.pack
      >>> parseFile parseOpts
      >=> runTransforms xforms bl
      >=> (\(bl', p) -> (bl',) <$> writeHtml5String (writeOpts bl' tpl) p)
      >=> _2 (return . T.unpack)
    )
    $ s
  where
    parseFile :: ReaderOptions -> Text -> PandocIO Pandoc
    parseFile opts =
      case bl^.format of
        Just "rst"      -> readRST      opts
        Just _          -> readMarkdown opts
        Nothing         ->
          case takeExtension (file' bl) of
            ".rst"  -> readRST opts
            ".rest" -> readRST opts
            ".txt"  -> readRST opts
            _       -> readMarkdown opts

    parseOpts = def
      { readerExtensions = pandocExtensions &
          foldr (.) id
            [ enableExtension Ext_tex_math_single_backslash
            , case bl^.litHaskell of
                Just False -> id
                _          -> enableExtension Ext_literate_haskell
            ]

            -- Relevant extensions enabled by default in pandocExtensions:
            -- (see https://hackage.haskell.org/package/pandoc-2.1.1/docs/src/Text-Pandoc-Extensions.html#pandocExtensions):

            -- Ext_smart
            -- Ext_yaml_metadata_block
            -- Ext_tex_math_dollars
            -- Ext_pandoc_title_block
            -- Ext_citations
      }
    writeOpts bl tpl = def
      { writerReferenceLinks  = True
      , writerWrapText        = WrapNone
      , writerTableOfContents = toc' bl
      , writerHTMLMathMethod  =
          case math' bl of
            ""  -> PlainMath
            opt -> mathOption (T.pack opt)
      , writerTemplate        = Just tpl
      }

    mathOption opt
      | opt `T.isPrefixOf` "mathml"      = MathML
      | opt `T.isPrefixOf` "mimetex"     =
          WebTeX (mathUrl "/cgi-bin/mimetex.cgi?" opt)
      | opt `T.isPrefixOf` "webtex"      = WebTeX (mathUrl webTeXURL opt)
      | opt `T.isPrefixOf` "mathjax"     = MathJax (mathUrl mathJaxURL opt)
      | otherwise                      = PlainMath

    webTeXURL  = "http://chart.apis.google.com/chart?cht=tx&chl="
    mathJaxURL = T.append "http://cdn.mathjax.org/mathjax/latest/MathJax.js"
                   "?config=TeX-AMS-MML_HTMLorMML"

    urlPart :: Text -> Text
    urlPart = T.drop 1 . T.dropWhile (/='=')

    mathUrl dflt opt  = case urlPart opt of "" -> dflt; x -> x

-- | Turn @CRLF@ pairs into a single @LF@.  This is necessary since
--   'readMarkdown' is picky about line endings.
fixLineEndings :: String -> String
fixLineEndings []             = []
fixLineEndings ('\r':'\n':cs) = '\n':fixLineEndings cs
fixLineEndings (c:cs)         = c:fixLineEndings cs

-- We use a special template with the "standalone" pandoc writer.  We
-- don't actually want truly "standalone" HTML documents because they
-- have to sit inside another web page.  But we do want things like
-- math typesetting and a table of contents.
blHtmlTemplate = T.unlines
  [ "$if(highlighting-css)$"
  , "  <style type=\"text/css\">"
  , "$highlighting-css$"
  , "  </style>"
  , "$endif$"
  , "$for(css)$"
  , "  <link rel=\"stylesheet\" href=\"$css$\" $if(html5)$$else$type=\"text/css\" $endif$/>"
  , "$endfor$"
  , "$if(math)$"
  , "  $math$"
  , "$endif$"
  , "$if(toc)$"
  , "<div id=\"$idprefix$TOC\">"
  , "$toc$"
  , "</div>"
  , "$endif$"
  , "$body$"
  ]
