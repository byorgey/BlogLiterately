{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.Options
-- Copyright   :  (c) 2008-2010 Robert Greayer, 2012 Brent Yorgey
-- License     :  GPL (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- Configuation and command-line options.
--
-----------------------------------------------------------------------------

module Text.BlogLiterately.Options
    where

import Control.Lens (makeLenses, view)
import Data.Monoid
import Data.Version
import Paths_BlogLiterately (version)

import System.Console.CmdArgs

import Text.BlogLiterately.Flag
import Text.BlogLiterately.Highlight


-- | Configuration record (and command-line options) for @BlogLiterately@.
data BlogLiterately = BlogLiterately
  { _style          :: Flag String        -- ^ Name of a style file
  , _hsHighlight    :: Flag HsHighlight   -- ^ Haskell highlighting mode
  , _otherHighlight :: Flag Bool          -- ^ Use highlighting-kate for
                                          --   non-Haskell?
  , _wplatex        :: Flag Bool          -- ^ Format LaTeX for WordPress?
  , _math           :: Flag String        -- ^ Indicate how to format math
  , _ghci           :: Flag Bool          -- ^ Automatically process ghci sessions?
  , _uploadImages   :: Flag Bool          -- ^ Automatically upload images?
  , _categories     :: [String]           -- ^ Categories for the post
  , _tags           :: [String]           -- ^ Tags for the post
  , _blogid         :: Flag String        -- ^ Blog-specific identifier
                                          --   (e.g. for blogging software
                                          --   handling multiple blogs)
  , _profile        :: Flag String        -- ^ Name of profile to use.
  , _blog           :: Flag String        -- ^ Blog xmlrpc URL
  , _user           :: Flag String        -- ^ Blog user name
  , _password       :: Flag String        -- ^ Blog password (omit to be interactively prompted)
  , _title          :: Flag String        -- ^ Post title
  , _file           :: Flag String        -- ^ File to post
  , _postid         :: Flag String        -- ^ ID of a post to update
  , _page           :: Flag Bool          -- ^ Create a \"page\" instead of a post
  , _publish        :: Flag Bool          -- ^ Should the post be published?
                                          --   (Otherwise it is uploaded as a draft.)
  , _xtra           :: [String]           -- ^ Extension arguments, for use e.g. by
                                          --   custom transforms
  }
  deriving (Show,Data,Typeable)

makeLenses ''BlogLiterately

instance Monoid BlogLiterately where
  mempty =
    BlogLiterately
    { _style          = mempty
    , _hsHighlight    = mempty
    , _otherHighlight = mempty
    , _wplatex        = mempty
    , _math           = mempty
    , _ghci           = mempty
    , _uploadImages   = mempty
    , _categories     = mempty
    , _tags           = mempty
    , _blogid         = mempty
    , _profile        = mempty
    , _blog           = mempty
    , _user           = mempty
    , _password       = mempty
    , _title          = mempty
    , _file           = mempty
    , _postid         = mempty
    , _page           = mempty
    , _publish        = mempty
    , _xtra           = mempty
    }

  mappend bl1 bl2 =
    BlogLiterately
    { _style          = combine _style
    , _hsHighlight    = combine _hsHighlight
    , _otherHighlight = combine _otherHighlight
    , _wplatex        = combine _wplatex
    , _math           = combine _math
    , _ghci           = combine _ghci
    , _uploadImages   = combine _uploadImages
    , _categories     = combine _categories
    , _tags           = combine _tags
    , _blogid         = combine _blogid
    , _profile        = combine _profile
    , _blog           = combine _blog
    , _user           = combine _user
    , _password       = combine _password
    , _title          = combine _title
    , _file           = combine _file
    , _postid         = combine _postid
    , _page           = combine _page
    , _publish        = combine _publish
    , _xtra           = combine _xtra
    }
    where combine f = f bl1 `mappend` f bl2

-- Some convenient accessors that include defaulting

style'          = fromFlag ""    . view style
hsHighlight'    = fromFlag (HsColourInline defaultStylePrefs) . view hsHighlight
otherHighlight' = fromFlag True  . view otherHighlight
wplatex'        = fromFlag False . view wplatex
math'           = fromFlag ""    . view math
ghci'           = fromFlag False . view ghci
uploadImages'   = fromFlag False . view uploadImages
blogid'         = fromFlag ""    . view blogid
profile'        = fromFlag ""    . view profile
blog'           = fromFlag ""    . view blog
user'           = fromFlag ""    . view user
password'       = fromFlag ""    . view password
title'          = fromFlag ""    . view title
file'           = fromFlag ""    . view file
postid'         = fromFlag ""    . view postid
page'           = fromFlag False . view page
publish'        = fromFlag False . view publish

-- | Command-line configuration for use with @cmdargs@.
blOpts :: BlogLiterately
blOpts = BlogLiterately
     { _style = def &= help "style specification (for --hscolour-icss)"
                    &= typFile
     , _hsHighlight = enum
       [ Flag (HsColourInline defaultStylePrefs)
         &= explicit
         &= name "hscolour-icss"
         &= help "highlight haskell: hscolour, inline style (default)"
       , Flag HsColourCSS
         &= explicit
         &= name "hscolour-css"
         &= help "highlight haskell: hscolour, separate stylesheet"
       , Flag HsNoHighlight
         &= explicit
         &= name "hs-nohighlight"
         &= help "no haskell highlighting"
       , Flag HsKate
         &= explicit
         &= name "hs-kate"
         &= help "highlight haskell with highlighting-kate"
       ]
     , _otherHighlight = enum
       [ Flag True
         &= explicit
         &= name "kate"
         &= help "highlight non-Haskell code with highlighting-kate (default)"
       , Flag False
         &= explicit
         &= name "no-kate"
         &= help "don't highlight non-Haskell code"
       ]
     , _wplatex = def &= help "reformat inline LaTeX the way WordPress expects"
     , _math    = def &= help "how to layout math, where --math=<pandoc-option>[=URL]"
     , _ghci    = def &= help "run [ghci] blocks through ghci and include output"
     , _uploadImages = def &= name "upload-images" &= explicit &= help "upload local images"
     , _page    = def &= help "create a \"page\" instead of a post (WordPress only)"
     , _publish = def &= help "publish post (otherwise it's uploaded as a draft)"
     , _categories = def
       &= explicit
       &= name "category"
       &= help "post category (can specify more than one)"
     , _tags = def
       &= explicit
       &= name "tag"
       &= help "tag (can specify more than one)"

     , _xtra = def
       &= help "extension arguments, for use with custom extensions"
     , _blogid   = def &= help "Blog specific identifier" &= typ "ID"
     , _postid   = def &= help "Post to replace (if any)" &= typ "ID"

     , _profile  = def &= typ "STRING"   &= help "profile to use"
     , _blog     = def &= typ "URL"      &= help "blog XML-RPC url (if omitted, html goes to stdout)"
     , _user     = def &= typ "USER"     &= help "user name"
     , _password = def &= typ "PASSWORD" &= help "password"
     , _title    = def &= typ "TITLE"    &= help "post title"
     , _file     = def &= argPos 0 &= typ "FILE"
  }
  &= program "BlogLiterately"
  &= summary ("BlogLierately v" ++ showVersion version ++ ", (c) Robert Greayer 2008-2010, Brent Yorgey 2012\n" ++
              "This program comes with ABSOLUTELY NO WARRANTY\n")
