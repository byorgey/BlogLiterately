{-# LANGUAGE PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.Ghci
-- Copyright   :  (c) 1997-2005 Ralf Hinze <ralf.hinze@comlab.ox.ac.uk>, Andres Loeh <lhs2tex@andres-loeh.de>, 2012 Brent Yorgey
-- License     :  GPL (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- Format specially marked blocks as interactive ghci sessions.  Uses
-- some ugly but effective code for interacting with an external ghci
-- process taken from lhs2TeX.
--
-----------------------------------------------------------------------------

module Text.BlogLiterately.Ghci
    (
    -- * Running ghci
      ProcessInfo
    , ghciEval
    , withGhciProcess
    , isLiterate
    , stopGhci

    -- * Extracting output
    -- $extract

    , magic
    , extract'
    , extract
    , breaks

    -- * Formatting
    , formatInlineGhci

    ) where

import Control.Arrow              ( first)
import Control.Monad.IO.Class     ( liftIO )
import Control.Monad.Trans.Reader ( ReaderT, runReaderT, ask )
import Data.Functor               ( (<$>) )
import Data.List                  ( isPrefixOf, intercalate )
import System.IO
import System.Process             ( ProcessHandle, waitForProcess
                                  , runInteractiveCommand )

import Text.Pandoc                ( Pandoc, Block(CodeBlock), bottomUpM )

import Text.BlogLiterately.Block  ( unTag )

-- | Information about a running process: stdin, stdout, stderr, and a
--   handle.
type ProcessInfo = (Handle, Handle, Handle, ProcessHandle)

-- | Evaluate an expression using an external @ghci@ process.
ghciEval :: String -> ReaderT ProcessInfo IO String
ghciEval expr =  do
  (pin, pout, _, _) <- ask
  let script = "putStrLn " ++ show magic ++ "\n"
                 ++ expr ++ "\n"
                 ++ "putStrLn " ++ show magic ++ "\n"
  liftIO $ do
    hPutStr pin script
    hFlush pin
    extract' pout

-- | Start an external ghci process, run a computation with access to
--   it, and finally stop the process.
withGhciProcess :: FilePath -> ReaderT ProcessInfo IO a -> IO a
withGhciProcess f m = do
  isLit <- isLiterate f
  pi    <- runInteractiveCommand $ "ghci -v0 -ignore-dot-ghci "
                                   ++ (if isLit then f else "")
  res   <- runReaderT m pi
  stopGhci pi
  return res

-- | Poor man's check to see whether we have a literate Haskell file.
isLiterate :: FilePath -> IO Bool
isLiterate f = (any ("> " `isPrefixOf`) . lines) <$> readFile f

-- | Stop a ghci process by passing it @:q@ and waiting for it to exit.
stopGhci :: ProcessInfo -> IO ()
stopGhci (pin,_,_,pid) = do
  hPutStrLn pin ":q"
  hFlush pin
  _ <- waitForProcess pid   -- ignore exit code
  return ()

-- $extract
-- To extract the answer from @ghci@'s output we use a simple technique
-- which should work in most cases: we print the string @magic@ before
-- and after the expression we are interested in. We assume that
-- everything that appears before the first occurrence of @magic@ on the
-- same line is the prompt, and everything between the first @magic@ and
-- the second @magic@ plus prompt is the result we look for.

-- | There is nothing magic about the magic string.
magic :: String
magic =  "!@#$^&*"

extract' :: Handle -> IO String
extract' h = fmap (extract . unlines) (readMagic 2)
  where
    readMagic :: Int -> IO [String]
    readMagic 0 = return []
    readMagic n = do
      l <- hGetLine h
      let n' | (null . snd . breaks (isPrefixOf magic)) l = n
             | otherwise                                  = n - 1
      fmap (l:) (readMagic n')

extract                       :: String -> String
extract s                     =  v
    where (t, u)              =  breaks (isPrefixOf magic) s
          -- t contains everything up to magic, u starts with magic
          -- |u'                      =  tail (dropWhile (/='\n') u)|
          pre                 =  reverse . takeWhile (/='\n') . reverse $ t
          prelength           =  if null pre then 0 else length pre + 1
          -- pre contains the prefix of magic on the same line
          u'                  =  drop (length magic + prelength) u
          -- we drop the magic string, plus the newline, plus the prefix
          (v, _)              =  breaks (isPrefixOf (pre ++ magic)) u'
          -- we look for the next occurrence of prefix plus magic

breaks                        :: ([a] -> Bool) -> [a] -> ([a], [a])
breaks p []                   =  ([], [])
breaks p as@(a : as')
    | p as                    =  ([], as)
    | otherwise               =  first (a:) $ breaks p as'

-- | Given the path to the @.lhs@ source and its representation as a
--   @Pandoc@ document, @formatInlineGhci@ finds any @[ghci]@ blocks
--   in it, runs them through @ghci@, and formats the results as an
--   interactive @ghci@ session.
formatInlineGhci :: FilePath -> Pandoc -> IO Pandoc
formatInlineGhci f = withGhciProcess f . bottomUpM formatInlineGhci'
  where
    formatInlineGhci' :: Block -> ReaderT ProcessInfo IO Block
    formatInlineGhci' b@(CodeBlock attr s)
      | Just "ghci" <- tag =  do
          results <- zip inputs <$> mapM ghciEval inputs
          return $ CodeBlock attr (intercalate "\n" $ map formatGhciResult results)

      | otherwise = return b

      where (tag,src) = unTag s
            inputs    = lines src

    formatInlineGhci' b = return b

    formatGhciResult (input, output)
      = "<span style=\"color: gray;\">ghci&gt;</span> " ++ input ++ (unlines . map ("  "++) . lines) output  -- XXX this should be configurable!
