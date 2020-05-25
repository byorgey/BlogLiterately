
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.LaTeX
-- Copyright   :  (c) 2012 Brent Yorgey
-- License     :  GPL (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- Utilities for working with embedded LaTeX.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Text.BlogLiterately.LaTeX
    (
      rawTeXify
    , wpTeXify
    ) where

import           Data.Text   (Text)
import qualified Data.Text   as T
import           Text.Pandoc

bracket :: Text -> Text -> Text -> Text
bracket l r t = T.append l (T.append t r)

-- | Pass LaTeX through unchanged.
rawTeXify :: Pandoc -> Pandoc
rawTeXify = bottomUp formatDisplayTex . bottomUp formatInlineTex
  where formatInlineTex :: [Inline] -> [Inline]
        formatInlineTex (Math InlineMath tex : is)
          = (RawInline (Format "html") (bracket "$" "$" tex)) : is
        formatInlineTex is = is

        formatDisplayTex :: [Block] -> [Block]
        formatDisplayTex (Para [Math DisplayMath tex] : bs)
          = RawBlock (Format "html") (bracket "\n\\[" "\\]\n" tex)
          : bs
        formatDisplayTex bs = bs

-- | WordPress can render LaTeX, but expects it in a special non-standard
--   format (@\$latex foo\$@).  The @wpTeXify@ function formats LaTeX code
--   using this format so that it can be processed by WordPress.
wpTeXify :: Pandoc -> Pandoc
wpTeXify = bottomUp formatDisplayTex . bottomUp formatInlineTex
  where formatInlineTex :: [Inline] -> [Inline]
        formatInlineTex (Math InlineMath tex : is)
          = (Str $ bracket "$latex " "$" (unPrefix "latex" tex)) : is
        formatInlineTex is = is

        formatDisplayTex :: [Block] -> [Block]
        formatDisplayTex (Para [Math DisplayMath tex] : bs)
          = RawBlock (Format "html") "<p><div style=\"text-align: center\">"
          : Plain [Str $ bracket "$latex \\displaystyle " "$" (unPrefix "latex" tex)]
          : RawBlock (Format "html") "</div></p>"
          : bs
        formatDisplayTex bs = bs

        unPrefix pre s
          | pre `T.isPrefixOf` s = T.drop (T.length pre) s
          | otherwise            = s
