{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

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
    ( -- * Transforms
      Transform(..), pureTransform, ioTransform, runTransform, runTransforms

      -- * Standard transforms
      -- $standard

    , standardTransforms

    , optionsXF
    , profileXF
    , highlightOptsXF
    , passwordXF
    , titleXF
    , wptexifyXF
    , ghciXF
    , imagesXF
    , highlightXF
    , centerImagesXF

      -- * Transforming documents
    , xformDoc

      -- * Utilities
    , fixLineEndings
    ) where

import           Control.Applicative               (pure, (<$>), (<**>))
import           Control.Arrow                     ((>>>))
import           Control.Lens                      (has, isn't, set, use, (%=),
                                                    (&), (.=), (.~), (^.), _1,
                                                    _2, _Just)
import           Control.Monad.State
import           Data.Default                      (def)
import           Data.List                         (intercalate, isPrefixOf)
import           Data.Monoid                       (mappend)
import           Data.Monoid                       (mempty, (<>))
import qualified Data.Set                          as S
import qualified Data.Traversable                  as T
import           System.Directory                  (doesFileExist,
                                                    getAppUserDataDirectory)
import           System.Exit                       (exitFailure)
import           System.FilePath                   ((<.>), (</>))
import           System.IO                         (hFlush, stdout)
import           Text.Blaze.Html.Renderer.String   (renderHtml)
import           Text.Pandoc
import           Text.Pandoc.Options
import           Text.Parsec                       (ParseError)

import           Text.BlogLiterately.Block         (onTag)
import           Text.BlogLiterately.Ghci          (formatInlineGhci)
import           Text.BlogLiterately.Highlight     (HsHighlight (HsColourInline),
                                                    colourisePandoc,
                                                    getStylePrefs,
                                                    _HsColourInline)
import           Text.BlogLiterately.Image         (uploadAllImages)
import           Text.BlogLiterately.LaTeX         (wpTeXify)
import           Text.BlogLiterately.Options
import           Text.BlogLiterately.Options.Parse (readBLOptions)

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
                 { getTransform :: StateT (BlogLiterately, Pandoc) IO ()
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
pureTransform transf cond = Transform (gets fst >>= \bl -> _2 %= transf bl) cond

-- | Construct a transformation from a function in the @IO@ monad.
ioTransform :: (BlogLiterately -> Pandoc -> IO Pandoc)
            -> (BlogLiterately -> Bool) -> Transform
ioTransform transf cond = Transform (StateT . fmap (fmap $ (,) ()) $ transf') cond
  where transf' (bl,p) = ((,) bl) <$> transf bl p

-- | Run a 'Transform' (if its condition is met).
runTransform :: Transform -> StateT (BlogLiterately, Pandoc) IO ()
runTransform t = do
  bl <- gets fst
  when (xfCond t bl) $ getTransform t

-- | Run a pipeline of 'Transform's.
runTransforms :: [Transform] -> BlogLiterately -> Pandoc -> IO (BlogLiterately, Pandoc)
runTransforms ts bl p = execStateT (mapM_ runTransform ts) (bl,p)

--------------------------------------------------
-- Standard transforms
--------------------------------------------------

-- $standard
-- These transforms are enabled by default in the standard
-- @BlogLiterately@ executable.

-- | Format embedded LaTeX for WordPress (if the @wplatex@ flag is set).
wptexifyXF :: Transform
wptexifyXF = pureTransform (const wpTeXify) wplatex'

-- | Format embedded @ghci@ sessions (if the @ghci@ flag is set).
ghciXF :: Transform
ghciXF = ioTransform (formatInlineGhci . file') ghci'

-- | Upload embedded local images to the server (if the @uploadImages@
--   flag is set).
imagesXF :: Transform
imagesXF = ioTransform uploadAllImages uploadImages'

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
    centerImage (img@(Para [Image altText (imgUrl, imgTitle)]) : bs) =
        RawBlock "html" "<div style=\"text-align: center;\">"
      : img
      : RawBlock "html" "</div>"
      : bs
    centerImage bs = bs

-- | Potentially extract a title from the metadata block, and set it
--   in the options record.
titleXF :: Transform
titleXF = Transform extractTitle (const True)
  where
    extractTitle = do
      (Pandoc (Meta t _ _) _) <- gets snd
      case t of
        [] -> return ()
        is ->
          -- title set explicitly with --title takes precedence.
          _1.title %= (`mplus` Just (intercalate " " [s | Str s <- is]))

-- | Extract blocks tagged with @[BLOpts]@ and use their contents as
--   options.
optionsXF :: Transform
optionsXF = Transform optionsXF' (const True)
  where
    optionsXF' = do
      p <- gets snd
      let (errs, opts) = queryWith extractOptions p
      mapM_ (liftIO . print) errs
      _1 %= (<> opts)

      let p' = bottomUp killOptionBlocks p
      _2 .= p'

-- XXX need to extract out some common functionality below.

-- | Take a block and extract from it a list of parse errors and an
--   options record.  If the blog is not tagged with @[BLOpts]@ these
--   will just be empty.
extractOptions :: Block -> ([ParseError], BlogLiterately)
extractOptions = onTag "blopts" (const readBLOptions) (const mempty)

-- | Delete any blocks tagged with @[BLOpts]@.
killOptionBlocks :: Block -> Block
killOptionBlocks = onTag "blopts" (const (const Null)) id

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

-- XXX finish writing about these

-- | The standard set of transforms that are run by default (in order
--   from top to bottom):
--
--   * 'optionsXF' -- extract options specified in a @[BLOpts]@ block in the file
--
--   * 'profileXF'
--
--   * 'highlightOptsXF'
--
--   * 'passwordXF'
--
--   * 'titleXF'
--
--   * 'wptexifyXF'
--
--   * 'ghciXF'
--
--   * 'imagesXF'
--
--   * 'highlightXF'
--
--   * 'centerImagesXF'
standardTransforms :: [Transform]
standardTransforms =
  [ optionsXF       -- has to go first since it may affect later transforms
  , profileXF
  , highlightOptsXF
  , passwordXF
  , titleXF
  , wptexifyXF
  , ghciXF
  , imagesXF
  , highlightXF
  , centerImagesXF
  ]

--------------------------------------------------
-- Transforming documents
--------------------------------------------------

-- | Transform a complete input document string to an HTML output
--   string, given a list of transformation passes.
xformDoc :: BlogLiterately -> [Transform] -> String -> IO (BlogLiterately, String)
xformDoc bl xforms =
        fixLineEndings
    >>> readMarkdown parseOpts

    >>> runTransforms xforms bl

    >=> _2 (return . writeHtml writeOpts)
    >=> _2 (return . renderHtml)
  where
    parseOpts = def
                { readerExtensions = Ext_literate_haskell
                                     `S.insert` readerExtensions def
                , readerSmart      = True
                }
    writeOpts = def
                { writerReferenceLinks = True
                , writerHTMLMathMethod =
                  case math' bl of
                    ""  -> PlainMath
                    opt -> mathOption opt }
    mathOption opt
      | opt `isPrefixOf` "latexmathml" ||
        opt `isPrefixOf` "asciimathml" = LaTeXMathML (mathUrlMaybe opt)
      | opt `isPrefixOf` "mathml"      = MathML (mathUrlMaybe opt)
      | opt `isPrefixOf` "mimetex"     =
          WebTeX (mathUrl "/cgi-bin/mimetex.cgi?" opt)
      | opt `isPrefixOf` "webtex"      = WebTeX (mathUrl webTeXURL opt)
      | opt `isPrefixOf` "jsmath"      = JsMath (mathUrlMaybe opt)
      | opt `isPrefixOf` "mathjax"     = MathJax (mathUrl mathJaxURL opt)
      | opt `isPrefixOf` "gladtex"     = GladTeX

    webTeXURL  = "http://chart.apis.google.com/chart?cht=tx&chl="
    mathJaxURL = "http://cdn.mathjax.org/mathjax/latest/MathJax.js"
                 ++ "?config=TeX-AMS-MML_HTMLorMML"

    urlPart = drop 1 . dropWhile (/='=')

    mathUrlMaybe opt = case urlPart opt of "" -> Nothing; x -> Just x
    mathUrl def opt  = case urlPart opt of "" -> def; x -> x

-- | Turn @CRLF@ pairs into a single @LF@.  This is necessary since
--   'readMarkdown' is picky about line endings.
fixLineEndings :: String -> String
fixLineEndings [] = []
fixLineEndings ('\r':'\n':cs) = '\n':fixLineEndings cs
fixLineEndings (c:cs) = c:fixLineEndings cs
