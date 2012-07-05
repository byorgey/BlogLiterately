{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.Transform
-- Copyright   :  (c) 2008-2010 Robert Greayer, 2012 Brent Yorgey
-- License     :  GPL (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- Tools for putting together a pipeline transforming the source for a
-- post into a completely formatted HTML document.
--
-----------------------------------------------------------------------------

module Text.BlogLiterately.Transform
    ( -- * Transforms
      Transform(..), runTransform, runTransforms

      -- * Standard transforms
    , wptexifyXF
    , ghciXF
    , imagesXF
    , highlightXF
    , standardTransforms

      -- * Transforming documents
    , xformDoc

      -- * Utilities
    , whenA, fixLineEndings
    ) where

import           Control.Arrow              ( first, (>>>), arr
                                            , Kleisli(..), runKleisli )
import qualified Control.Category as C      ( Category, id )
import qualified Data.Traversable as T

import           Text.Pandoc
import           Text.Blaze.Html.Renderer.String      ( renderHtml )

import           Text.BlogLiterately.Ghci
import           Text.BlogLiterately.Highlight
import           Text.BlogLiterately.Image
import           Text.BlogLiterately.LaTeX
import           Text.BlogLiterately.Options

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
--   The transformation is then specified as a @'Kleisli' IO 'Pandoc'
--   'Pandoc'@ arrow, which is isomorphic to @Pandoc -> IO Pandoc@.  If
--   you have a pure function of type @Pandoc -> Pandoc@, wrap it in a
--   call to 'arr' to produce a 'Kleisli' arrow.  If you have a
--   function @Pandoc -> IO Pandoc@, wrap it in the 'Kleisli'
--   constructor.
--
--   For examples, see the implementations of the standard transforms
--   below.
data Transform = Transform
                 { getTransform :: BlogLiterately -> Kleisli IO Pandoc Pandoc
                   -- ^ A document transformation, which can depend on
                   --   BlogLiterately options and can have effects in
                   --   the @IO@ monad.
                 , xfCond       :: BlogLiterately -> Bool
                   -- ^ A condition under which to run the transformation.
                 }

-- | Run a 'Transform' (if its condition is met).
runTransform :: Transform -> BlogLiterately -> Kleisli IO Pandoc Pandoc
runTransform t bl = getTransform t bl `whenA` xfCond t bl

-- | Run a pipeline of 'Transform's.
runTransforms :: [Transform] -> BlogLiterately -> Kleisli IO Pandoc Pandoc
runTransforms ts = foldr (>>>) (C.id) . T.traverse runTransform ts

-- | Format embedded LaTeX for WordPress (if the @wplatex@ flag is set).
wptexifyXF :: Transform
wptexifyXF = Transform (const (arr wpTeXify)) wplatex

-- | Format embedded @ghci@ sessions (if the @ghci@ flag is set).
ghciXF :: Transform
ghciXF = Transform (Kleisli . formatInlineGhci . file) ghci

-- | Upload embedded local images to the server (if the @uploadImages@
--   flag is set).
imagesXF :: Transform
imagesXF = Transform (Kleisli . uploadAllImages) uploadImages

-- | Perform syntax highlighting on code blocks.
highlightXF :: Transform
highlightXF = Transform
  (\bl -> arr (colourisePandoc (hsHighlight bl) (otherHighlight bl)))
  (const True)

-- | The standard set of transforms that are run by default:
--   'wptexifyXF', 'ghciXF', 'imagesXF', 'highlightXF'.
standardTransforms :: [Transform]
standardTransforms = [wptexifyXF, ghciXF, imagesXF, highlightXF]

-- | Transform a complete input document string to an HTML output
--   string, given a list of transformation passes.
xformDoc :: BlogLiterately -> [Transform] -> (String -> IO String)
xformDoc bl xforms = runKleisli $
        arr     fixLineEndings
    >>> arr     (readMarkdown parseOpts)

    >>> runTransforms xforms bl

    >>> arr     (writeHtml writeOpts)
    >>> arr     renderHtml
  where
    writeOpts = defaultWriterOptions
                { writerReferenceLinks = True }
    parseOpts = defaultParserState
                { stateLiterateHaskell = True }

-- | Turn @CRLF@ pairs into a single @LF@.  This is necessary since
--   'readMarkdown' is picky about line endings.
fixLineEndings :: String -> String
fixLineEndings [] = []
fixLineEndings ('\r':'\n':cs) = '\n':fixLineEndings cs
fixLineEndings (c:cs) = c:fixLineEndings cs

-- | A useful arrow utility for running some part of a pipeline
-- conditionally.
whenA :: C.Category (~>) => (a ~> a) -> Bool -> (a ~> a)
whenA a p | p         = a
          | otherwise = C.id
