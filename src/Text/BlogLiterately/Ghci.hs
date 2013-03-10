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

import           Control.Arrow              (first)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Char                  (isSpace)
import           Data.Functor               ((<$>))
import           Data.List                  (intercalate, isPrefixOf)
import           System.IO
import           System.Process             (ProcessHandle,
                                             runInteractiveCommand,
                                             waitForProcess)

import           Data.List.Split
import           Text.Pandoc                (Block (CodeBlock), Pandoc,
                                             bottomUpM)

import           Text.BlogLiterately.Block  (onTag)

-- | Information about a running process: stdin, stdout, stderr, and a
--   handle.
type ProcessInfo = (Handle, Handle, Handle, ProcessHandle)

-- | An input to ghci consists of an expression/command, possibly
--   paired with an expected output.
data GhciInput  = GhciInput { expr :: String, expected :: Maybe String }
  deriving Show

-- | An output from ghci is either a correct output, or an incorrect
--   (unexpected) output paired with the expected output.
data GhciOutput = OK String
                | Unexpected String String
  deriving Show

-- | A @GhciLine@ is a @GhciInput@ paired with its corresponding @GhciOutput@.
data GhciLine = GhciLine GhciInput GhciOutput
  deriving Show

-- | Evaluate an expression using an external @ghci@ process.
ghciEval :: GhciInput -> ReaderT ProcessInfo IO GhciOutput
ghciEval (GhciInput expr expected) =  do
  (pin, pout, _, _) <- ask
  let script = "putStrLn " ++ show magic ++ "\n"
                 ++ expr ++ "\n"
                 ++ "putStrLn " ++ show magic ++ "\n"
  out <- liftIO $ do
    hPutStr pin script
    hFlush pin
    extract' pout
  let out' = strip out
  case expected of
    Nothing -> return $ OK out'
    Just exp
      | out' == exp -> return $ OK out'
      | otherwise   -> return $ Unexpected out' exp

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
--
--   Lines beginning in the first column of the block are interpreted
--   as inputs.  Lines indented by one or more space are interpreted
--   as /expected outputs/.  Consecutive indented lines are
--   interpreted as one multi-line expected output, with a number of
--   spaces removed from the beginning of each line equal to the
--   number of spaces at the start of the first indented line.
--
--   If the output for a given input is the same as the expected
--   output (or if no expected output is given), the result is typeset
--   normally.  If the actual and expected outputs differ, the actual
--   output is typeset first in red, then the expected output in blue.
formatInlineGhci :: FilePath -> Pandoc -> IO Pandoc
formatInlineGhci f = withGhciProcess f . bottomUpM formatInlineGhci'
  where
    formatInlineGhci' :: Block -> ReaderT ProcessInfo IO Block
    formatInlineGhci' = onTag "ghci" formatGhciBlock return

    formatGhciBlock attr src = do
      let inputs = parseGhciInputs src
      results <- zipWith GhciLine inputs <$> mapM ghciEval inputs
      return $ CodeBlock attr (intercalate "\n" $ map formatGhciResult results)

parseGhciInputs :: String -> [GhciInput]
parseGhciInputs = map mkGhciInput
                . split
                  ( dropInitBlank
                  . dropFinalBlank
                  . keepDelimsL
                  $ whenElt (not . (" " `isPrefixOf`))
                  )
                . lines

mkGhciInput :: [String] -> GhciInput
mkGhciInput [i]     = GhciInput i Nothing
mkGhciInput (i:exp) = GhciInput i (Just . unlines' . unindent $ exp)

unlines' :: [String] -> String
unlines' = intercalate "\n"

strip :: String -> String
strip = f . f
  where f = dropWhile isSpace . reverse

unindent :: [String] -> [String]
unindent (x:xs) = map (drop indentAmt) (x:xs)
  where indentAmt = length . takeWhile (==' ') $ x

indent :: Int -> String -> String
indent n = unlines' . map (replicate n ' '++) . lines

colored color txt = "<span style=\"color: " ++ color ++ ";\">" ++ txt ++ "</span>"
coloredBlock color = unlines' . map (colored color) . lines

ghciPrompt = colored "gray" "ghci&gt; "

formatGhciResult (GhciLine (GhciInput input _) (OK output))
  | all isSpace output
    = ghciPrompt ++ esc input
  | otherwise
    = ghciPrompt ++ esc input ++ "\n" ++ indent 2 (esc output) ++ "\n"
formatGhciResult (GhciLine (GhciInput input _) (Unexpected output exp))
  = ghciPrompt ++ esc input ++ "\n" ++ indent 2 (coloredBlock "red" (esc output))
                            ++ "\n" ++ indent 2 (coloredBlock "blue" (esc exp))
                            ++ "\n"

    -- XXX the styles above should be configurable...

esc :: String -> String
esc = concatMap escapeOne
  where
    escapeOne '<' = "&lt;"
    escapeOne '>' = "&gt;"
    escapeOne  c  = [c]
