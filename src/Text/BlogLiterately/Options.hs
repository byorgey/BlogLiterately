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
    (
      -- * BlogLiterately options record
      BlogLiterately(..), blOpts

      -- ** Lenses
      -- $lenses

    , style
    , hsHighlight
    , otherHighlight
    , wplatex
    , math
    , ghci
    , uploadImages
    , categories
    , tags
    , blogid
    , profile
    , blog
    , user
    , password
    , title
    , file
    , postid
    , page
    , publish
    , htmlOnly
    , xtra

    -- ** Default accessors
    -- $defaccess

    , style'
    , hsHighlight'
    , otherHighlight'
    , wplatex'
    , math'
    , ghci'
    , uploadImages'
    , blogid'
    , profile'
    , blog'
    , user'
    , password'
    , title'
    , file'
    , postid'
    , page'
    , publish'
    , htmlOnly'
    )
    where

import           Control.Lens                  (makeLenses, view)
import           Control.Monad                 (mplus)
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid
import           Data.Version
import           Paths_BlogLiterately          (version)

import           System.Console.CmdArgs

import           Text.BlogLiterately.Highlight

-- | Configuration record (and command-line options) for @BlogLiterately@.
data BlogLiterately = BlogLiterately
  { _style          :: Maybe String        -- ^ Name of a style file
  , _hsHighlight    :: Maybe HsHighlight   -- ^ Haskell highlighting mode
  , _otherHighlight :: Maybe Bool          -- ^ Use highlighting-kate for
                                           --   non-Haskell?
  , _wplatex        :: Maybe Bool          -- ^ Format LaTeX for WordPress?
  , _math           :: Maybe String        -- ^ Indicate how to format math
  , _ghci           :: Maybe Bool          -- ^ Automatically process ghci sessions?
  , _uploadImages   :: Maybe Bool          -- ^ Automatically upload images?
  , _categories     :: [String]            -- ^ Categories for the post
  , _tags           :: [String]            -- ^ Tags for the post
  , _blogid         :: Maybe String        -- ^ Blog-specific identifier
                                           --   (e.g. for blogging software
                                           --   handling multiple blogs)
  , _profile        :: Maybe String        -- ^ Name of profile to use.
  , _blog           :: Maybe String        -- ^ Blog xmlrpc URL
  , _user           :: Maybe String        -- ^ Blog user name
  , _password       :: Maybe String        -- ^ Blog password (omit to be interactively prompted)
  , _title          :: Maybe String        -- ^ Post title
  , _file           :: Maybe String        -- ^ File to post
  , _postid         :: Maybe String        -- ^ ID of a post to update
  , _page           :: Maybe Bool          -- ^ Create a \"page\" instead of a post
  , _publish        :: Maybe Bool          -- ^ Should the post be published?
                                           --   (Otherwise it is uploaded as a draft.)
  , _htmlOnly       :: Maybe Bool          -- ^ Don't upload anything;
                                           --   just output HTML to
                                           --   stdout.
  , _xtra           :: [String]            -- ^ Extension arguments, for use e.g. by
                                           --   custom transforms
  }
  deriving (Show,Data,Typeable)

-- $lenses
-- We derive lenses for all the @BlogLiterately@ fields using the
-- @lens@ library.

makeLenses ''BlogLiterately

instance Monoid BlogLiterately where
  mempty =
    BlogLiterately
    { _style          = Nothing
    , _hsHighlight    = Nothing
    , _otherHighlight = Nothing
    , _wplatex        = Nothing
    , _math           = Nothing
    , _ghci           = Nothing
    , _uploadImages   = Nothing
    , _categories     = []
    , _tags           = []
    , _blogid         = Nothing
    , _profile        = Nothing
    , _blog           = Nothing
    , _user           = Nothing
    , _password       = Nothing
    , _title          = Nothing
    , _file           = Nothing
    , _postid         = Nothing
    , _page           = Nothing
    , _publish        = Nothing
    , _htmlOnly       = Nothing
    , _xtra           = []
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
    , _htmlOnly       = combine _htmlOnly
    , _xtra           = combine _xtra
    }
    where combine f = f bl1 `mplus` f bl2

--------------------------------------------------
-- Default accessors
--------------------------------------------------

-- $defaccess
-- Some convenient accessors that strip off the Maybe and return an
-- appropriate default value.

style'          = fromMaybe ""    . view style
hsHighlight'    = fromMaybe (HsColourInline defaultStylePrefs) . view hsHighlight
otherHighlight' = fromMaybe True  . view otherHighlight
wplatex'        = fromMaybe False . view wplatex
math'           = fromMaybe ""    . view math
ghci'           = fromMaybe False . view ghci
uploadImages'   = fromMaybe False . view uploadImages
blogid'         = fromMaybe "default" . view blogid
profile'        = fromMaybe ""    . view profile
blog'           = fromMaybe ""    . view blog
user'           = fromMaybe ""    . view user
password'       = fromMaybe ""    . view password
title'          = fromMaybe ""    . view title
file'           = fromMaybe ""    . view file
postid'         = fromMaybe ""    . view postid
page'           = fromMaybe False . view page
publish'        = fromMaybe False . view publish
htmlOnly'       = fromMaybe False . view htmlOnly

-- | Command-line configuration for use with @cmdargs@.
blOpts :: BlogLiterately
blOpts = BlogLiterately
     { _style = def &= help "style specification (for --hscolour-icss)"
                    &= typFile
                    &= name "style" &= name "s" &= explicit
     , _hsHighlight = enum
       [ Just (HsColourInline defaultStylePrefs)
         &= explicit
         &= name "hscolour-icss"
         &= help "highlight haskell: hscolour, inline style (default)"
       , Just HsColourCSS
         &= explicit
         &= name "hscolour-css"
         &= help "highlight haskell: hscolour, separate stylesheet"
       , Just HsNoHighlight
         &= explicit
         &= name "hs-nohighlight"
         &= help "no haskell highlighting"
       , Just HsKate
         &= explicit
         &= name "hs-kate"
         &= help "highlight haskell with highlighting-kate"
       ]
     , _otherHighlight = enum
       [ Just True
         &= explicit
         &= name "kate"
         &= help "highlight non-Haskell code with highlighting-kate (default)"
       , Just False
         &= explicit
         &= name "no-kate"
         &= help "don't highlight non-Haskell code"
       ]
     , _wplatex = def &= help "reformat inline LaTeX the way WordPress expects"
                  &= name "wplatex" &= name "w" &= explicit
     , _math    = def &= help "how to layout math, where --math=<pandoc-option>[=URL]"
                  &= name "math" &= name "m" &= explicit
     , _ghci    = def &= help "run [ghci] blocks through ghci and include output"
                  &= name "ghci" &= name "g" &= explicit
     , _uploadImages = def &= name "upload-images" &= name "I" &= explicit &= help "upload local images"
     , _page    = def &= help "create a \"page\" instead of a post (WordPress only)"
                  &= name "page" &= explicit
     , _publish = def &= help "publish post (otherwise it's uploaded as a draft)"
                  &= name "publish" &= explicit
     , _htmlOnly = def &= help "don't upload anything; output HTML to stdout"
                  &= name "html-only" &= name "h" &= explicit
     , _categories = def
       &= explicit
       &= name "category" &= name "C"
       &= help "post category (can specify more than one)"
     , _tags = def
       &= explicit
       &= name "tag" &= name "T"
       &= help "tag (can specify more than one)"

     , _xtra     = def
                   &= help "extension arguments, for use with custom extensions"
                   &= name "xtra" &= name "x" &= explicit
     , _blogid   = def &= help "Blog specific identifier" &= typ "ID"
                   &= name "blogid" &= explicit
     , _postid   = def &= help "Post to replace (if any)" &= typ "ID"
                   &= name "postid" &= name "i" &= explicit
     , _profile  = def &= typ "STRING"   &= help "profile to use"
                   &= name "profile" &= name "P" &= explicit
     , _blog     = def &= typ "URL"      &= help "blog XML-RPC url (if omitted, HTML goes to stdout)"
                   &= name "blog" &= name "b" &= explicit
     , _user     = def &= typ "USER"     &= help "user name"
                   &= name "user" &= name "u" &= explicit
     , _password = def &= typ "PASSWORD" &= help "password"
                   &= name "password" &= name "p" &= explicit
     , _title    = def &= typ "TITLE"    &= help "post title"
                   &= name "title" &= name "t" &= explicit
     , _file     = def &= argPos 0 &= typ "FILE"
  }
  &= program "BlogLiterately"
  &= summary ("BlogLierately v" ++ showVersion version ++ ", (c) Robert Greayer 2008-2010, Brent Yorgey 2012-2013\n" ++
              "For help, see http://byorgey.wordpress.com/blogliterately/")
