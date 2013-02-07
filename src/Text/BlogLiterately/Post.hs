{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.Post
-- Copyright   :  (c) 2008-2010 Robert Greayer, 2012 Brent Yorgey
-- License     :  GPL (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- Uploading posts to the server.
--
-----------------------------------------------------------------------------

module Text.BlogLiterately.Post
    (
      mkPost, mkArray, postIt
    ) where

import Control.Lens                         ( (^.) )
import Control.Monad                        ( unless )
import Data.Maybe                           ( fromMaybe )

import Network.XmlRpc.Client                ( remote )
import Network.XmlRpc.Internals             ( Value(..), toValue, XmlRpcType )

import Text.BlogLiterately.Options

{-
The metaWeblog API defines `newPost` and `editPost` procedures that
look like:

    [other]
    metaWeblog.newPost (blogid, username, password, struct, publish)
        returns string
    metaWeblog.editPost (postid, username, password, struct, publish)
        returns true

For WordPress blogs, the `blogid` is ignored.  The user name and
password are simply strings, and `publish` is a flag indicating
whether to load the post as a draft, or to make it public immediately.
The `postid` is an identifier string which is assigned when you
initially create a post. The interesting bit is the `struct` field,
which is an XML-RPC structure defining the post along with some
meta-data, like the title.  I want be able to provide the post body, a
title, and lists of categories and tags.  For the body and title, we
could just let HaXR convert the values automatically into the XML-RPC
`Value` type, since they all have the same Haskell type (`String`) and
thus can be put into a list.  But the categories and tags are lists of
strings, so we need to explicitly convert everything to a `Value`,
then combine:
-}

-- | Prepare a post for uploading by creating something of the proper
--   form to be an argument to an XML-RPC call.
mkPost :: String    -- ^ Post title
       -> String    -- ^ Post content
       -> [String]  -- ^ List of categories
       -> [String]  -- ^ List of tags
       -> Bool      -- ^ @True@ = page, @False@ = post
       -> [(String, Value)]
mkPost title text categories tags page =
       mkArray "categories" categories
    ++ mkArray "mt_keywords" tags
    ++ [ ("title", toValue title)
       , ("description", toValue text)
       ]
    ++ [ ("post_type", toValue "page") | page ]

-- | Given a name and a list of values, create a named \"array\" field
--   suitable for inclusion in an XML-RPC struct.
mkArray :: XmlRpcType [a] => String -> [a] -> [(String, Value)]
mkArray _    []     = []
mkArray name values = [(name, toValue values)]

{-
The HaXR library exports a function for invoking XML-RPC procedures:

    [haskell]
    remote :: Remote a =>
        String -- ^ Server URL. May contain username and password on
               --   the format username:password\@ before the hostname.
           -> String -- ^ Remote method name.
           -> a      -- ^ Any function
         -- @(XmlRpcType t1, ..., XmlRpcType tn, XmlRpcType r) =>
                     -- t1 -> ... -> tn -> IO r@

The function requires an URL and a method name, and returns a function
of type `Remote a => a`.  Based on the instances defined for `Remote`,
any function with zero or more parameters in the class `XmlRpcType`
and a return type of `XmlRpcType r => IO r` will work, which means you
can simply 'feed' `remote` additional arguments as required by the
remote procedure, and as long as you make the call in an IO context,
it will typecheck.  `postIt` calls `metaWeblog.newPost` or
`metaWeblog.editPost` (or simply prints the HTML to stdout) as
appropriate:
-}

-- | Given a configuration and a formatted post, upload it to the server.
postIt :: BlogLiterately -> String -> IO ()
postIt bl html =
  case bl^.blog of
    Nothing  -> putStr html
    Just url -> do
      let pwd = password' bl
      case bl^.postid of
        Nothing  -> do
          pid <- remote url "metaWeblog.newPost"
                   (blogid' bl)
                   (user' bl)
                   pwd
                   post
                   (publish' bl)
          putStrLn $ "Post ID: " ++ pid
        Just pid -> do
          success <- remote url "metaWeblog.editPost" pid
                       (user' bl)
                       pwd
                       post
                       (publish' bl)
          unless success $ putStrLn "update failed!"
  where
    post = mkPost
             (title' bl)
             html (bl^.categories) (bl^.tags)
             (page' bl)
