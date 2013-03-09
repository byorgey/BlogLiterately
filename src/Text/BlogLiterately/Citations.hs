
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.Citations
-- Copyright   :  (c) 2012 Brent Yorgey
-- License     :  GPL (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- Formatting citations in pandoc documents.
--
-----------------------------------------------------------------------------

module Text.BlogLiterately.Citations
    (
      formatCitations
    ) where

--
formatCitations :: [FilePath] -> Pandoc -> IO Pandoc
formatCitations doc
          if citeMethod == Citeproc && not (null refs)
             then do
                csldir <- getAppUserDataDirectory "csl"
                cslfile' <- if null cslfile
                               then findDataFile datadir "default.csl"
                               else do
                                  ex <- doesFileExist cslfile
                                  if ex
                                     then return cslfile
                                     else findDataFile datadir $
                                            replaceDirectory
                                            (replaceExtension cslfile "csl")
                                            csldir
                processBiblio cslfile' cslabbrevs refs doc1
