{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

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
      mkPost, postIt
    ) where

import           Control.Lens                (at, makePrisms, to, traverse,
                                              (^.), (^?))
import qualified Data.Map                    as M

import           Network.XmlRpc.Client       (remote)
import           Network.XmlRpc.Internals    (Value (..), XmlRpcType, toValue)

import           Text.BlogLiterately.Options

{-
The WordPress XML-RPC API defines `newPost` and `editPost` procedures that
look like:

    [other]
    wp.newPost (blogid, username, password, content)
        returns string (post_id)
    wp.editPost (blogid, username, password, post_id, content)
        returns true

The blogid is a number uniquely identifying a wordpress blog.  The
user name and password are simply strings.  The `post_id` is an
identifier string which is assigned when you initially create a
post. The interesting bit is the `content` field, which is an XML-RPC
structure defining the post along with some meta-data, like the title.
I want be able to provide the post body, a title, lists of categories
and tags, and whether the post should be published or a draft.
-}

-- | Prepare a post for uploading by creating something of the proper
--   form to be an argument to an XML-RPC call.
mkPost :: String    -- ^ Post title
       -> String    -- ^ Post content
       -> [String]  -- ^ List of categories
       -> [String]  -- ^ List of tags
       -> Bool      -- ^ @True@ = page, @False@ = post
       -> Bool      -- ^ @True@ = publish, @False@ = draft
       -> [(String, Value)]
mkPost title_ text_ categories_ tags_ page_ publish_ =
       [ ("post_title", toValue title_)
       , ("post_content", toValue text_)
       , ("post_status", toValue (if publish_ then "publish" else "draft"))
       , ("term", toValue $
                    [ ("category", toValue categories_)
                    , ("post_tag", toValue tags_)
                    ]
         )
       ]
    ++ [ ("post_type", toValue "page") | page_ ]

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
it will typecheck.  `postIt` calls `wp.newPost` or `wp.editPost` (or
simply prints the HTML to stdout) as appropriate:
-}

makePrisms ''Value

-- | Get the URL for a given post.
getPostURL :: String -> Int -> String -> String -> String -> IO (Maybe String)
getPostURL url bid pid usr pwd = do
  v <- remote url "wp.getPost" bid usr pwd pid
  return (v ^? _ValueStruct . to M.fromList . at "link" . traverse . _ValueString)

-- | Given a configuration and a formatted post, upload it to the server.
postIt :: BlogLiterately -> String -> IO ()
postIt bl html =
  case (bl^.blog, bl^.htmlOnly) of
    (Nothing  , _         ) -> putStr html
    (_        , Just True ) -> putStr html
    (Just url , _         ) -> do
      let pwd = password' bl
      case bl^.postid of
        Nothing  -> do
          pid <- remote url "wp.newPost"
                   (blogid' bl)
                   (user' bl)
                   pwd
                   post
          putStrLn $ "Post ID: " ++ pid
          getPostURL url (blogid' bl) pid (user' bl) pwd >>= maybe (return ()) putStrLn
        Just pid -> do
          success <- remote url "wp.editPost"
                       (blogid' bl)
                       (user' bl)
                       pwd
                       pid
                       post
          case success of
            True  -> getPostURL url (blogid' bl) pid (user' bl) pwd >>= maybe (return ()) putStrLn
            False -> putStrLn "Update failed!"
  where
    post = mkPost
             (title' bl)
             html (bl^.categories) (bl^.tags)
             (page' bl)
             (publish' bl)
