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
    , litHaskell
    , toc
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
    , format
    , postid
    , page
    , publish
    , htmlOnly
    , citations
    , xtra

    -- ** Default accessors
    -- $defaccess

    , style'
    , hsHighlight'
    , otherHighlight'
    , litHaskell'
    , toc'
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
    , format'
    , postid'
    , page'
    , publish'
    , htmlOnly'
    , citations'
    )
    where

import           Control.Lens                  (makeLenses, view)
import           Control.Monad                 (mplus)
import           Data.Maybe                    (fromMaybe)
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
  , _litHaskell     :: Maybe Bool          -- ^ Parse as literate Haskell?
  , _toc            :: Maybe Bool          -- ^ Generate a table of contents?
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
  , _format         :: Maybe String        -- ^ Format of the file
                                           --   (currently supported:
                                           --   markdown, rst)
  , _postid         :: Maybe String        -- ^ ID of a post to update
  , _page           :: Maybe Bool          -- ^ Create a \"page\" instead of a post
  , _publish        :: Maybe Bool          -- ^ Should the post be published?
                                           --   (Otherwise it is uploaded as a draft.)
  , _htmlOnly       :: Maybe Bool          -- ^ Don't upload anything;
                                           --   just output HTML to
                                           --   stdout.
  , _citations      :: Maybe Bool          -- ^ Process citations? (default: true)
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
    , _litHaskell     = Nothing
    , _toc            = Nothing
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
    , _format         = Nothing
    , _postid         = Nothing
    , _page           = Nothing
    , _publish        = Nothing
    , _htmlOnly       = Nothing
    , _citations      = Nothing
    , _xtra           = []
    }

  mappend bl1 bl2 =
    BlogLiterately
    { _style          = combine _style
    , _hsHighlight    = combine _hsHighlight
    , _otherHighlight = combine _otherHighlight
    , _litHaskell     = combine _litHaskell
    , _toc            = combine _toc
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
    , _format         = combine _format
    , _postid         = combine _postid
    , _page           = combine _page
    , _publish        = combine _publish
    , _htmlOnly       = combine _htmlOnly
    , _citations      = combine _citations
    , _xtra           = combine _xtra
    }
    where combine f = f bl1 `mplus` f bl2

--------------------------------------------------
-- Default accessors
--------------------------------------------------

-- $defaccess
-- Some convenient accessors that strip off the Maybe and return an
-- appropriate default value.

style' :: BlogLiterately -> String
style'          = fromMaybe ""    . view style

hsHighlight' :: BlogLiterately -> HsHighlight
hsHighlight'    = fromMaybe HsColourInline . view hsHighlight

otherHighlight' :: BlogLiterately -> Bool
otherHighlight' = fromMaybe True  . view otherHighlight

litHaskell' :: BlogLiterately -> Bool
litHaskell' = fromMaybe True . view litHaskell

toc' :: BlogLiterately -> Bool
toc'            = fromMaybe False . view toc

wplatex' :: BlogLiterately -> Bool
wplatex'        = fromMaybe False . view wplatex

math' :: BlogLiterately -> String
math'           = fromMaybe ""    . view math

ghci' :: BlogLiterately -> Bool
ghci'           = fromMaybe False . view ghci

uploadImages' :: BlogLiterately -> Bool
uploadImages'   = fromMaybe False . view uploadImages

blogid' :: BlogLiterately -> String
blogid'         = fromMaybe "default" . view blogid

profile' :: BlogLiterately -> String
profile'        = fromMaybe ""    . view profile

blog' :: BlogLiterately -> String
blog'           = fromMaybe ""    . view blog

user' :: BlogLiterately -> String
user'           = fromMaybe ""    . view user

password' :: BlogLiterately -> String
password'       = fromMaybe ""    . view password

title' :: BlogLiterately -> String
title'          = fromMaybe ""    . view title

file' :: BlogLiterately -> String
file'           = fromMaybe ""    . view file

format' :: BlogLiterately -> String
format'         = fromMaybe ""    . view format

postid' :: BlogLiterately -> String
postid'         = fromMaybe ""    . view postid

page' :: BlogLiterately -> Bool
page'           = fromMaybe False . view page

publish' :: BlogLiterately -> Bool
publish'        = fromMaybe False . view publish

htmlOnly' :: BlogLiterately -> Bool
htmlOnly'       = fromMaybe False . view htmlOnly

citations' :: BlogLiterately -> Bool
citations' = fromMaybe True . view citations

-- | Command-line configuration for use with @cmdargs@.
blOpts :: BlogLiterately
blOpts = BlogLiterately
     { _style = def &= help "style specification (for --hscolour-icss)"
                    &= typFile
                    &= name "style" &= name "s" &= explicit
     , _hsHighlight = enum
       [ Nothing
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
       [ Nothing
         &= explicit
         &= name "kate"
         &= help "highlight non-Haskell code with highlighting-kate (default)"
       , Just False
         &= explicit
         &= name "no-kate"
         &= help "don't highlight non-Haskell code"
       ]
     , _toc = enum
       [ Nothing
         &= name "no-toc"
         &= help "don't generate a table of contents (default)"
         &= explicit
       , Just True
         &= name "toc"
         &= help "generate a table of contents"
         &= explicit
       ]
     , _wplatex = def &= help "reformat inline LaTeX the way WordPress expects"
                  &= name "wplatex" &= name "w" &= explicit
     , _math    = def &= help "how to layout math, where --math=<pandoc-option>[=URL]"
                  &= name "math" &= name "m" &= explicit
     , _litHaskell = enum
        [ Nothing
          &= help "parse as literate Haskell (default)"
          &= name "lit-haskell"
          &= explicit
        , Just False
          &= help "do not parse as literate Haskell"
          &= name "no-lit-haskell"
          &= explicit
        ]
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

     , _citations = enum
        [ Nothing
          &= help "process citations (default)"
          &= name "citations"
          &= explicit
        , Just False
          &= help "do not process citations"
          &= name "no-citations"
          &= explicit
        ]

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
     , _format   = def &= typ "FORMAT"   &= help "input format: markdown or rst"
                   &= name "format" &= name "f" &= explicit
     , _file     = def &= argPos 0 &= typ "FILE"
  }
  &= program "BlogLiterately"
  &= summary ("BlogLierately v" ++ showVersion version ++ ", (c) Robert Greayer 2008-2010, Brent Yorgey 2012-2013\n" ++
              "For help, see http://byorgey.wordpress.com/blogliterately/")
