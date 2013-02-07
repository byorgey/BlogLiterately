{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.Flag
-- Copyright   :  (c) 2013 Brent Yorgey
-- License     :  GPL (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- Helper type for dealing with configuration flags.
--
-----------------------------------------------------------------------------

module Text.BlogLiterately.Flag where

import Control.Lens         (makePrisms)
import Data.Maybe           (fromMaybe, maybe)
import Data.Monoid
import System.Console.CmdArgs

data Flag a = NoFlag
            | Flag a
  deriving (Eq, Ord, Show, Data, Typeable)

makePrisms ''Flag

instance Monoid (Flag a) where
  mempty = NoFlag
  f `mappend` NoFlag = f
  _ `mappend` f      = f

instance Default (Flag a) where
  def = NoFlag

flagToMaybe :: Flag a -> Maybe a
flagToMaybe NoFlag   = Nothing
flagToMaybe (Flag a) = Just a

maybeToFlag :: Maybe a -> Flag a
maybeToFlag Nothing  = NoFlag
maybeToFlag (Just a) = Flag a

flag :: b -> (a -> b) -> Flag a -> b
flag a f = maybe a f . flagToMaybe

fromFlag :: a -> Flag a -> a
fromFlag a = fromMaybe a . flagToMaybe

