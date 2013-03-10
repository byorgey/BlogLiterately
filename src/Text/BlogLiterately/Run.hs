{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.Run
-- Copyright   :  (c) 2012-2013 Brent Yorgey
-- License     :  GPL (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- Functions for creating @BlogLiterately@ executables.  By default,
-- installing this library results in the installation of a standard
-- executable, called @BlogLiterately@, which corresponds to
-- 'blogLiterately' from this module.  However, you can create your
-- own custom executables with extra custom functionality using
-- 'blogLiteratelyWith' or 'blogLiteratelyCustom'.  For example:
--
-- > module Main where
-- > import Text.BlogLiterately
-- >
-- > myCustomXF = Transform ...
-- > main = blogLiteratelyWith [myCustomXF1, myCustomXF2]
--
-- See "Text.BlogLiterately.Transform" for examples of transforms,
-- additional transforms which are not enabled by default, and help in
-- creating your own.
--
-----------------------------------------------------------------------------

module Text.BlogLiterately.Run
    (
      blogLiterately
    , blogLiteratelyWith
    , blogLiteratelyCustom

    ) where

import           Control.Lens                  (set, use, (%=), (&), (.=), (.~),
                                                (^.))
import           System.Console.CmdArgs        (cmdArgs)
import qualified System.IO.UTF8                as U (readFile)

import           Text.BlogLiterately.Options   (blOpts, file')
import           Text.BlogLiterately.Post      (postIt)
import           Text.BlogLiterately.Transform (Transform, standardTransforms,
                                                xformDoc)

-- | The default BlogLiterately application.
blogLiterately :: IO ()
blogLiterately = blogLiteratelyCustom standardTransforms

-- | Like 'blogLiterately', but with the ability to specify additional
-- 'Transform's which will be applied /after/ the standard ones.
blogLiteratelyWith :: [Transform] -> IO ()
blogLiteratelyWith = blogLiteratelyCustom . (standardTransforms ++)

-- | Like 'blogLiterately', but with the ability to /replace/ the
--   standard 'Transform's.  Use this to implement custom interleaving
--   orders of the standard transforms and your own, to exclude some
--   or all of the standard transforms, etc.
blogLiteratelyCustom :: [Transform] -> IO ()
blogLiteratelyCustom ts =
      cmdArgs blOpts
  >>= \bl -> U.readFile (file' bl)
  >>= xformDoc bl ts
  >>= uncurry postIt
