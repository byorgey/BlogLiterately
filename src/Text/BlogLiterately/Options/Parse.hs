-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.Options.Parse
-- Copyright   :  (c) 2013 Brent Yorgey
-- License     :  GPL (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- Parsing configuration options from special @[BLOpts]@ blocks.
--
-----------------------------------------------------------------------------

module Text.BlogLiterately.Options.Parse
    ( readBLOptions
    , readBLOption
    , parseBLOption
    ) where

import           Control.Applicative         (pure, (*>), (<$>), (<*))
import           Control.Arrow               (second)
import           Control.Lens                (ASetter', (&), (.~))
import           Data.Char                   (isSpace)
import           Data.Either                 (partitionEithers)
import           Data.Monoid                 (Monoid, mconcat, mempty)
import           Text.Parsec                 (ParseError, char, many, noneOf,
                                              optional, parse, sepBy, spaces,
                                              string, try, (<|>))
import           Text.Parsec.Language        (haskell)
import           Text.Parsec.String          (Parser)
import           Text.Parsec.Token           (stringLiteral)


import           Text.BlogLiterately.Options

--------------------------------------------------
-- Parsing options
--------------------------------------------------

-- | Convert the contents of a @[BLOpts]@ block into an options record
--   and a list of parse errors.
readBLOptions :: String -> ([ParseError], BlogLiterately)
readBLOptions = second mconcat
              . partitionEithers
              . map (readBLOption . dropWhile isSpace)
              . filter (not . all isSpace)
              . lines

-- | Read a single line from a @[BLOpts]@ block.
readBLOption :: String -> Either ParseError BlogLiterately
readBLOption = parse parseBLOption ""

-- | Parse a single line from a @[BLOpts]@ block.
parseBLOption :: Parser BlogLiterately
parseBLOption =
      parseField style        "style"         parseStr
  <|> parseField toc          "toc"           parseBool
  <|> parseField wplatex      "wplatex"       parseBool
  <|> parseField math         "math"          parseStr
  <|> parseField litHaskell   "lit-haskell"   parseBool
  <|> parseField ghci         "ghci"          parseBool
  <|> parseField uploadImages "upload-images" parseBool
  <|> parseField categories   "categories"    parseStrList
  <|> parseField tags         "tags"          parseStrList
  <|> parseField blogid       "blogid"        parseStr
  <|> parseField profile      "profile"       parseStr
  <|> parseField blog         "blog"          parseStr
  <|> parseField user         "user"          parseStr
  <|> parseField password     "password"      parseStr
  <|> parseField title        "title"         parseStr
  <|> parseField postid       "postid"        parseStr
  <|> parseField page         "page"          parseBool
  <|> parseField publish      "publish"       parseBool
  <|> parseField htmlOnly     "html-only"     parseBool
  <|> parseField xtra         "xtras"         parseStrList

str :: Parser String
str = stringLiteral haskell <|> many (noneOf " \t\n\r,\"[]")

parseStr :: Parser (Maybe String)
parseStr = Just <$> str

parseBool :: Parser (Maybe Bool)
parseBool = Just <$> ( ((string "true"  <|> try (string "on")) *> pure True)
                   <|> ((string "false" <|>      string "off") *> pure False)
                     )

parseStrList :: Parser [String]
parseStrList = optional (char '[') *> paddedStr `sepBy` (char ',') <* optional (char ']')
  where
    paddedStr = spaces *> str <* spaces

parseField :: ASetter' BlogLiterately a -> String -> Parser a -> Parser BlogLiterately
parseField fld name p = do
  _ <- try (string name)
  spaces
  _ <- char '='
  spaces
  value <- p
  return (mempty & fld .~ value)
