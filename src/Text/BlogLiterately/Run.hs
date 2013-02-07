{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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
-- > main = blogLiteratelyWith [myCustomXF, centerImagesXF]
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

import           Control.Applicative
import           Control.Lens           ( set, use, (&), (.~), (.=), (%=), (^.) )
import           Control.Monad.State
import           Data.Monoid
import           System.Console.CmdArgs ( cmdArgs)
import           System.Directory       ( getAppUserDataDirectory, doesFileExist )
import           System.Exit
import           System.FilePath        ( (</>), (<.>) )
import           System.IO              ( hFlush, stdout )
import qualified System.IO.UTF8 as U    ( readFile )

import qualified Data.Configurator as Conf
import           Data.Configurator.Types (Config)

import           Text.BlogLiterately.Flag
import           Text.BlogLiterately.Highlight
import           Text.BlogLiterately.Options
import           Text.BlogLiterately.Post
import           Text.BlogLiterately.Transform

-- | The default BlogLiterately application.
blogLiterately :: IO ()
blogLiterately = blogLiteratelyCustom standardTransforms

-- | Like 'blogLiterately', but with the ability to specify additional
-- 'Transform's which will be applied /after/ the standard ones.
blogLiteratelyWith :: [Transform] -> IO ()
blogLiteratelyWith ts = blogLiteratelyCustom (standardTransforms ++ ts)

-- | Like 'blogLiterately', but with the ability to /replace/ the
--   standard 'Transform's.  Use this to implement custom interleaving
--   orders of the standard transforms and your own, to exclude some
--   or all of the standard transforms, etc.
blogLiteratelyCustom :: [Transform] -> IO ()
blogLiteratelyCustom ts = do
    bl <- loadProfile =<< cmdArgs blOpts

    flip evalStateT bl $ do
      prefs <- (liftIO . getStylePrefs) =<< use style
      hsHighlight %= Flag . flag (HsColourInline prefs)
                                 (_HsColourInline .~ prefs)

      b <- use blog
      p <- use password
      p' <- case (b,p) of
        (Flag _, NoFlag) -> liftIO passwordPrompt
        _                -> return p
      password .= p'

      f <- gets file'
      bl <- get
      html <- liftIO $ xformDoc bl ts =<< U.readFile f
      liftIO $ postIt bl html

passwordPrompt :: IO (Flag String)
passwordPrompt = do
  putStr "Password: " >> hFlush stdout
  pwd <- getLine
  return $ Flag pwd

loadProfile :: BlogLiterately -> IO BlogLiterately
loadProfile bl =
  case bl^.profile of
    NoFlag           -> return bl
    Flag profileName -> do
      appDir <- getAppUserDataDirectory "BlogLiterately"

      let profileCfg = appDir </> profileName <.> "cfg"
      e <- doesFileExist profileCfg
      case e of
        False -> do
          putStrLn $ profileCfg ++ ": file not found"
          exitFailure
        True  -> do
          p <- profileToBL =<< Conf.load [Conf.Required profileCfg]
          return $ mappend p bl

profileToBL :: Config -> IO BlogLiterately
profileToBL c = pure mempty
  <**> style       <.~> lookupFlag "style"
--  <**> hsHighlight <.~> undefined
  <**> otherHighlight <.~> lookupFlag "otherHighlight"
  <**> wplatex <.~> lookupFlag "wplatex"
  <**> math <.~> lookupFlag "math"
  <**> ghci <.~> lookupFlag "ghci"
  <**> uploadImages <.~> lookupFlag "uploadImages"
  <**> categories <.~> lookupList "categories"

  where
    lookupFlag n = maybeToFlag <$> Conf.lookup c n
    lookupList n = undefined -- Conf.lookupDefault
    f <.~> x = set f <$> x


