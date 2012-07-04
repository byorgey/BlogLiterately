
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.Block
-- Copyright   :  (c) 2012 Brent Yorgey
-- License     :  GPL (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- XXX write me
--
-----------------------------------------------------------------------------

module Text.BlogLiterately.Block
    (
      unTag
    ) where

import Text.ParserCombinators.Parsec

unTag :: String -> (String, String)
unTag s = either (const ("",s)) id $ parse tag "" s
  where
    tag = do
      tg <- between (char '[') (char ']') $ many $ noneOf "[]"
      skipMany $ oneOf " \t"
      (string "\r\n" <|> string "\n")
      txt <- many $ anyToken
      eof
      return (tg,txt)
