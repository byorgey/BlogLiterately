{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.Image
-- Copyright   :  (c) 2012 Brent Yorgey
-- License     :  GPL (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- Uploading images embedded in posts to the server.
--
-----------------------------------------------------------------------------

module Text.BlogLiterately.Image
    (
      uploadAllImages
    , uploadIt
    , mkMediaObject
    ) where

import           Control.Arrow              ( first, (>>>), arr
                                            , Kleisli(..), runKleisli )
import qualified Control.Category as C      ( Category, id )
import           Control.Monad              ( liftM, unless )
import           Control.Monad.IO.Class     ( liftIO )
import           Control.Monad.Trans.Reader ( ReaderT, runReaderT, ask )
import qualified Data.ByteString.Char8 as B
import           Data.Char                  ( toLower )
import           Data.Functor               ( (<$>) )
import           Data.List                  ( isPrefixOf, intercalate )
import           Data.Maybe                 ( fromMaybe )
import           System.FilePath            ( takeFileName, takeExtension )
import           System.IO
import qualified System.IO.UTF8 as U        ( readFile )
import           System.Process             ( ProcessHandle, waitForProcess
                                            , runInteractiveCommand )
import           Text.Pandoc
import           Network.XmlRpc.Client      ( remote )
import           Network.XmlRpc.Internals   ( Value(..), toValue )

import           Text.BlogLiterately.Options

-- | Transform a document by uploading any \"local\" images to the
--   server, and replacing their filenames with the URLs returned by the
--   server.
uploadAllImages :: BlogLiterately -> (Pandoc -> IO Pandoc)
uploadAllImages bl@(BlogLiterately{..}) =
  case blog of
    Just xmlrpc -> bottomUpM (uploadOneImage xmlrpc)
    _           -> return
  where
    uploadOneImage :: String -> Inline -> IO Inline
    uploadOneImage xmlrpc i@(Image altText (imgUrl, imgTitle))
      | isLocal imgUrl = do
          res <- uploadIt xmlrpc imgUrl bl
          case res of
            ValueStruct (lookup "url" -> Just (ValueString newUrl)) ->
              return $ Image altText (newUrl, imgTitle)
            _ -> do
              putStrLn $ "Warning: upload of " ++ imgUrl ++ " failed."
              return i
      | otherwise      = return i
    uploadOneImage _ i = return i

    isLocal imgUrl = none (`isPrefixOf` imgUrl) ["http", "/"]
    none p = all (not . p)

-- | Upload a file using the @metaWeblog.newMediaObject@ XML-RPC method
--   call.
uploadIt :: String -> FilePath -> BlogLiterately -> IO Value
uploadIt url filePath (BlogLiterately{..}) = do
  putStrLn $ "Uploading " ++ filePath ++ "..."
  media <- mkMediaObject filePath
  remote url "metaWeblog.newMediaObject" blogid user (fromMaybe "" password) media

-- | Prepare a file for upload.
mkMediaObject :: FilePath -> IO Value
mkMediaObject filePath = do
  bits <- B.unpack <$> B.readFile filePath
  return $ ValueStruct
    [ ("name", toValue fileName)
    , ("type", toValue fileType)
    , ("bits", ValueBase64 bits)
    ]
  where
    fileName = takeFileName filePath
    fileType = case (map toLower . drop 1 . takeExtension) fileName of
                 "png"  -> "image/png"
                 "jpg"  -> "image/jpeg"
                 "jpeg" -> "image/jpeg"
                 "gif"  -> "image/gif"
