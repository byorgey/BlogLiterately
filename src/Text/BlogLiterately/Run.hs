{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

import           Control.Applicative
import           Control.Lens                  (set, use, (%=), (&), (.=), (.~),
                                                (^.))
import           Control.Monad.State
import           Data.Monoid
import           System.Console.CmdArgs        (cmdArgs)
import           System.Directory              (doesFileExist,
                                                getAppUserDataDirectory)
import           System.Exit
import           System.FilePath               ((<.>), (</>))
import           System.IO                     (hFlush, stdout)
import qualified System.IO.UTF8                as U (readFile)

import qualified Data.Configurator             as Conf
import           Data.Configurator.Types       (Config, Configured (..), Name,
                                                Value (..))

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
      hsHighlight %= Just . maybe (HsColourInline prefs)
                                  (_HsColourInline .~ prefs)

      b <- use blog
      p <- use password
      p' <- case (b,p) of
        (Just _, Nothing) -> liftIO passwordPrompt
        _                 -> return p
      password .= p'

      f <- gets file'
      bl <- get
      (bl',html) <- liftIO $ xformDoc bl ts =<< U.readFile f
      liftIO $ postIt bl' html

passwordPrompt :: IO (Maybe String)
passwordPrompt = do
  putStr "Password: " >> hFlush stdout
  pwd <- getLine
  return $ Just pwd

loadProfile :: BlogLiterately -> IO BlogLiterately
loadProfile bl =
  case bl^.profile of
    Nothing          -> return bl
    Just profileName -> do
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
  <**> style          <.~> lookupV    "style"
--  <**> hsHighlight  <.~> undefined
--  <**> otherHighlight <.~> lookupV    "otherHighlight"
  <**> wplatex        <.~> lookupV    "wplatex"
  <**> math           <.~> lookupV    "math"
  <**> ghci           <.~> lookupV    "ghci"
  <**> uploadImages   <.~> lookupV    "upload-images"
  <**> categories     <.~> lookupList "categories"
  <**> tags           <.~> lookupList "tags"
  <**> blogid         <.~> lookupV    "blogid"
  <**> blog           <.~> lookupV    "blog"
  <**> user           <.~> lookupV    "user"
  <**> password       <.~> lookupV    "password"
  <**> title          <.~> lookupV    "title"
  <**> postid         <.~> lookupV    "postid"
  <**> page           <.~> lookupV    "page"
  <**> publish        <.~> lookupV    "publish"
  <**> xtra           <.~> lookupList "xtra"

  where
    lookupV :: Configured a => Name -> IO (Maybe a)
    lookupV = Conf.lookup c

--    lookupList :: Configured [a] => Name -> IO [a]
    lookupList = Conf.lookupDefault [] c

--    (<.~>) :: Functor f => ASetter s t a b -> f b -> f (s -> t)
    f <.~> x = set f <$> x

instance Configured [String] where
  convert (List vs) = mapM convert vs
  convert _         = Nothing
