
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
    , onTag
    ) where

import           Data.Text        (Text)
import qualified Data.Text        as T
import           Text.Pandoc      (Attr, Block (CodeBlock))
import           Text.Parsec
import           Text.Parsec.Text

-- switch to megaparsec, use e.g. https://hackage.haskell.org/package/megaparsec-8.0.0/docs/Text-Megaparsec.html#v:takeWhileP ?

-- | Given a block, if it begins with a tag in square brackets, strip off
--   the tag and return a pair consisting of the tag and de-tagged
--   block.  Otherwise, return @Nothing@ and the unchanged block.
unTag :: Text -> (Maybe Text, Text)
unTag s = either (const (Nothing, s)) id $ parse tag "" s
  where
    tag :: Parser (Maybe Text, Text)
    tag = do
      tg <- T.pack <$> (between (char '[') (char ']') $ many $ noneOf "[]")
      skipMany $ oneOf " \t"
      _   <- (string "\r\n" <|> string "\n")
      txt <- T.pack <$> many anyToken
      eof
      return (Just tg, txt)

-- | Run the given function on the attributes and source code of code
--   blocks with a tag matching the given tag (case insensitive).  On
--   any other blocks (which don't have a matching tag, or are not code
--   blocks), run the other function.
onTag :: Text -> (Attr -> Text -> a) -> (Block -> a) -> Block -> a
onTag t f def b@(CodeBlock attr@(_, as, _) s)
  | T.toLower t `elem` map T.toLower (maybe id (:) tag $ as)
    = f attr src
  | otherwise = def b
  where (tag, src) = unTag s
onTag _ _ def b = def b
