
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

module Text.BlogLiterately.LaTeX
    (
      wpTeXify
    ) where

import Data.List   ( isPrefixOf )
import Text.Pandoc

-- | WordPress can render LaTeX, but expects it in a special non-standard
--   format (@\$latex foo\$@).  The @wpTeXify@ function formats LaTeX code
--   using this format so that it can be processed by WordPress.
wpTeXify :: Pandoc -> Pandoc
wpTeXify = bottomUp formatDisplayTex . bottomUp formatInlineTex
  where formatInlineTex :: [Inline] -> [Inline]
        formatInlineTex (Math InlineMath tex : is)
          = (Str $ "$latex " ++ unPrefix "latex" tex ++ "$") : is
        formatInlineTex is = is

        formatDisplayTex :: [Block] -> [Block]
        formatDisplayTex (Para [Math DisplayMath tex] : bs)
          = RawBlock "html" "<p><div style=\"text-align: center\">"
          : Plain [Str $ "$latex " ++ "\\displaystyle " ++ unPrefix "latex" tex ++ "$"]
          : RawBlock "html" "</div></p>"
          : bs
        formatDisplayTex bs = bs

        unPrefix pre s
          | pre `isPrefixOf` s = drop (length pre) s
          | otherwise          = s
