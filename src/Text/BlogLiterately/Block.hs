
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.Block
-- Copyright   :  (c) 2008-2010 Robert Greayer, 2012 Brent Yorgey
-- License     :  GPL (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- Utilities for working with code blocks.
--
-----------------------------------------------------------------------------

module Text.BlogLiterately.Block
    (
      unTag
    ) where

import Text.ParserCombinators.Parsec

-- | Given a block, if begins with a tag in square brackets, strip off
--   the tag and return a pair consisting of the tag and de-tagged
--   block.  Otherwise, return @Nothing@ and the unchanged block.
unTag :: String -> (Maybe String, String)
unTag s = either (const (Nothing, s)) id $ parse tag "" s
  where
    tag = do
      tg <- between (char '[') (char ']') $ many $ noneOf "[]"
      skipMany $ oneOf " \t"
      (string "\r\n" <|> string "\n")
      txt <- many $ anyToken
      eof
      return (Just tg, txt)
