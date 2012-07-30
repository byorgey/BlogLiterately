{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.Run
-- Copyright   :  (c) 2012 Brent Yorgey
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
-- > main = blogLiteratelyWith [myCustomXF]
--
-- See "Text.BlogLiterately.Transform" for examples of transforms and
-- help in creating your own.
--
-----------------------------------------------------------------------------

module Text.BlogLiterately.Run
    (
      blogLiterately
    , blogLiteratelyWith
    , blogLiteratelyCustom

    ) where

import           System.Console.CmdArgs ( cmdArgs)
import           System.IO              ( hFlush, stdout )
import qualified System.IO.UTF8 as U    ( readFile )

import Text.BlogLiterately.Highlight
import Text.BlogLiterately.Options
import Text.BlogLiterately.Post
import Text.BlogLiterately.Transform

-- | The default BlogLiterately application.
blogLiterately :: IO ()
blogLiterately = blogLiteratelyCustom standardTransforms

-- | Like 'blogLiterately', but with the ability to specify custom
-- 'Transform's which will be applied /after/ the standard ones.
blogLiteratelyWith :: [Transform] -> IO ()
blogLiteratelyWith ts = blogLiteratelyCustom (standardTransforms ++ ts)

-- | Like 'blogLiterately', but with the ability to /replace/ the
--   standard 'Transform's with your own.  Use this to implement
--   custom interleaving orders of the standard transforms and your
--   own, to exclude some or all of the standard transforms, etc.
blogLiteratelyCustom :: [Transform] -> IO ()
blogLiteratelyCustom ts = do
    bl <- cmdArgs blOpts
    let (BlogLiterately{..}) = bl

    prefs <- getStylePrefs style
    let hsHighlight' = case hsHighlight of
            HsColourInline _ -> HsColourInline prefs
            _                -> hsHighlight
        bl' = bl { hsHighlight = hsHighlight' }

    pwd <- case (blog, password) of
      (Just _, Nothing) -> passwordPrompt
      _                 -> return password
    let bl'' = bl' { password = pwd }

    html <- xformDoc bl'' ts =<< U.readFile file
    postIt bl'' html

passwordPrompt :: IO (Maybe String)
passwordPrompt = do
  putStr "Password: " >> hFlush stdout
  pwd <- getLine
  return $ Just pwd