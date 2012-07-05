{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.Options
-- Copyright   :  (c) 2008-2010 Robert Greayer, 2012 Brent Yorgey
-- License     :  GPL (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- XXX write me
--
-----------------------------------------------------------------------------

module Text.BlogLiterately.Options
    ( BlogLiterately(..)
    , bl
    )
    where

import System.Console.CmdArgs

import Text.BlogLiterately.Highlight

-- | Configuration record (and command-line options) for @BlogLiterately@.
data BlogLiterately = BlogLiterately
  { style          :: String        -- ^ name of a style file
  , hsHighlight    :: HsHighlight   -- ^ Haskell highlighting mode
  , otherHighlight :: Bool          -- ^ use highlighting-kate for
                                    --   non-Haskell?
  , wplatex        :: Bool          -- ^ format LaTeX for WordPress?
  , ghci           :: Bool          -- ^ automatically generate ghci sessions?
  , uploadImages   :: Bool          -- ^ automatically upload images?
  , categories     :: [String]      -- ^ categories for the post
  , tags           :: [String]      -- ^ tags for the post
  , blogid         :: String        -- ^ blog-specific identifier
                                    --   (e.g. for blogging software
                                    --   handling multiple blogs)
  , blog           :: Maybe String  -- ^ blog xmlrpc URL
  , user           :: String        -- ^ blog user name
  , password       :: String        -- ^ blog password
  , title          :: String        -- ^ post title
  , file           :: String        -- ^ file to post
  , postid         :: Maybe String  -- ^ id of a post to update
  , page           :: Bool          -- ^ create a "page" instead of a post
  , publish        :: Bool          -- ^ Should the post be published,
                                    --   or loaded as a draft?
  }
  deriving (Show,Data,Typeable)

-- | Command-line configuration for use with cmdargs.
bl :: BlogLiterately
bl = BlogLiterately
     { style = ""  &= help "style specification (for --hscolour-icss)"
                   &= typFile
     , hsHighlight = enum
       [ (HsColourInline defaultStylePrefs)
         &= explicit
         &= name "hscolour-icss"
         &= help "highlight haskell: hscolour, inline style (default)"
       , HsColourCSS
         &= explicit
         &= name "hscolour-css"
         &= help "highlight haskell: hscolour, separate stylesheet"
       , HsNoHighlight
         &= explicit
         &= name "hs-nohighlight"
         &= help "no haskell highlighting"
       , HsKate
         &= explicit
         &= name "hs-kate"
         &= help "highlight haskell with highlighting-kate"
       ]
     , otherHighlight = enum
       [ True
         &= explicit
         &= name "other-kate"
         &= help "highlight other code with highlighting-kate"
       ]
     , wplatex = def &= help "reformat inline LaTeX the way WordPress expects"
     , ghci    = def &= help "run [ghci] blocks through ghci and include output"
     , uploadImages = def &= name "upload-images" &= explicit &= help "upload local images"
     , page    = def &= help "create a \"page\" instead of a post (WordPress only)"
     , publish = def &= help "publish post (otherwise it's uploaded as a draft)"
     , categories = def
       &= explicit
       &= name "category"
       &= help "post category (can specify more than one)"
     , tags = def
       &= explicit
       &= name "tag"
       &= help "tag (can specify more than one)"

     , blogid   = "default" &= help "Blog specific identifier" &= typ "ID"
     , postid   = def &= help "Post to replace (if any)" &= typ "ID"

     , blog     = def &= typ "URL"      &= help "blog XML-RPC url (if omitted, html goes to stdout)"
     , user     = def &= typ "USER"     &= help "user name"
     , password = def &= typ "PASSWORD" &= help "password"
     , title    = def &= typ "TITLE"    &= help "post title"
     , file     = def &= argPos 0 &= typ "FILE"
  }
  &= program "BlogLiterately"
  &= summary ("BlogLierately v0.4, (c) Robert Greayer 2008-2010, Brent Yorgey 2012\n" ++
              "This program comes with ABSOLUTELY NO WARRANTY\n")
