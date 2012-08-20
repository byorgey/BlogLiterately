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

import           Control.Arrow              ( first, second, (>>>), arr
                                            , Kleisli(..), runKleisli )
import qualified Control.Category as C      ( Category, id )
import           Control.Monad              ( liftM, unless )
import           Control.Monad.IO.Class     ( liftIO )
import           Control.Monad.Trans.Class  ( lift )
import           Control.Monad.Trans.Reader ( ReaderT, runReaderT, ask )
import           Control.Monad.Trans.State  ( StateT, runStateT, get, modify )
import qualified Data.ByteString.Char8 as B
import           Data.Char                  ( toLower )
import           Data.Functor               ( (<$>) )
import           Data.List                  ( isPrefixOf, intercalate )
import qualified Data.Map as M
import           Data.Maybe                 ( fromMaybe )
import           System.Directory           ( doesFileExist )
import           System.FilePath            ( takeFileName, takeExtension )
import           System.IO
import qualified System.IO.UTF8 as U        ( readFile )
import           System.Process             ( ProcessHandle, waitForProcess
                                            , runInteractiveCommand )

import           Text.Pandoc
import           Network.XmlRpc.Client      ( remote )
import           Network.XmlRpc.Internals   ( Value(..), toValue )

import           Text.BlogLiterately.Options

type URL = String

-- | Transform a document by uploading any \"local\" images to the
--   server, and replacing their filenames with the URLs returned by
--   the server.  Only upload any given image once (determined by file
--   name), even across runs: uploaded images and their associated URL
--   on the server is tracked in a special dotfile,
--   @.BlogLiterately-uploaded-images@.
uploadAllImages :: BlogLiterately -> Pandoc -> IO Pandoc
uploadAllImages bl@(BlogLiterately{..}) p =
  case blog of
    Just xmlrpc -> do
      uploaded <- readUploadedImages
      (p', uploaded') <- runStateT (bottomUpM (uploadOneImage xmlrpc) p) uploaded
      writeUploadedImages uploaded'
      return p'
    _           -> return p
  where
    uploadOneImage :: String -> Inline -> StateT (M.Map FilePath URL) IO Inline
    uploadOneImage xmlrpc i@(Image altText (imgUrl, imgTitle))
      | isLocal imgUrl = do
          uploaded <- get
          case M.lookup imgUrl uploaded of
            Just url -> return $ Image altText (url, imgTitle)
            Nothing  -> do
              res <- lift $ uploadIt xmlrpc imgUrl bl
              case res of
                Just (ValueStruct (lookup "url" -> Just (ValueString newUrl))) -> do
                  modify (M.insert imgUrl newUrl)
                  return $ Image altText (newUrl, imgTitle)
                _ -> do
                  liftIO . putStrLn $ "Warning: upload of " ++ imgUrl ++ " failed."
                  return i
      | otherwise      = return i
    uploadOneImage _ i = return i

    isLocal imgUrl = none (`isPrefixOf` imgUrl) ["http", "/"]
    none p = all (not . p)

uploadedImagesFile = ".BlogLiterately-uploaded-images"

-- | Read the list of previously uploaded images and their associated URLs from
--   a special dotfile (namely, @.BlogLiterately-uploaded-images@).
readUploadedImages :: IO (M.Map FilePath URL)
readUploadedImages = do
  e <- doesFileExist uploadedImagesFile
  case e of
    False -> return M.empty
    True  -> do
      txt <- readFile uploadedImagesFile
      let m = fromMaybe (M.empty) (readMay txt)
      length txt `seq` return m

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
              [(a,"")] -> Just a
              _        -> Nothing

-- | Write out the list of uploaded images and their associated URLs
--   to a special dotfile (namely, @.BlogLiterately-uploaded-images@).
writeUploadedImages :: M.Map FilePath URL -> IO ()
writeUploadedImages m = writeFile uploadedImagesFile (show m)

-- | Upload a file using the @metaWeblog.newMediaObject@ XML-RPC method
--   call.
uploadIt :: String -> FilePath -> BlogLiterately -> IO (Maybe Value)
uploadIt url filePath (BlogLiterately{..}) = do
  putStr $ "Uploading " ++ filePath ++ "..."
  mmedia <- mkMediaObject filePath
  case mmedia of
    Nothing -> do
      putStrLn $ "\nFile not found: " ++ filePath
      return Nothing
    Just media -> do
      val <- remote url "metaWeblog.newMediaObject" blogid user (fromMaybe "" password) media
      putStrLn "done."
      return $ Just val

-- | Prepare a file for upload.
mkMediaObject :: FilePath -> IO (Maybe Value)
mkMediaObject filePath = do
  exists <- doesFileExist filePath
  if not exists
    then return Nothing
    else do
      bits <- B.readFile filePath
      return . Just $ ValueStruct
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
