{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.BlogLiterately.Highlight
-- Copyright   :  (c) 2008-2010 Robert Greayer, 2012 Brent Yorgey
-- License     :  GPL (see LICENSE)
-- Maintainer  :  Brent Yorgey <byorgey@gmail.com>
--
-- Syntax highlighting.
--
-----------------------------------------------------------------------------

module Text.BlogLiterately.Highlight
    ( HsHighlight(..)
    , _HsColourInline
    , colourIt
    , litify
    , StylePrefs
    , defaultStylePrefs
    , getStylePrefs
    , bakeStyles
    , replaceBreaks
    , colouriseCodeBlock
    , colourisePandoc
    ) where

-- xmlParse (from HaXmL) uses String
-- hscolour uses String

import           Data.Text                           (Text)
import qualified Data.Text                           as T

import           Control.Lens                        (makePrisms)
import           Control.Monad                       (liftM)
import           Data.Char                           (toLower)
import           Data.List                           (find)
import           Data.Maybe                          (fromMaybe)

import           Language.Haskell.HsColour           (Output (..), hscolour)
import           Language.Haskell.HsColour.Colourise (defaultColourPrefs)
import           System.Console.CmdArgs              (Data, Typeable)
import           Text.Blaze.Html.Renderer.String     (renderHtml)
import           Text.Highlighting.Kate
import           Text.Pandoc.Definition
import           Text.Pandoc.Shared                  (safeRead)
import           Text.XML.HaXml                      hiding (attr, find, html)
import           Text.XML.HaXml.Posn                 (noPos)

import           Text.BlogLiterately.Block           (unTag)

-- | Style preferences are specified as a list of mappings from class
--   attributes to CSS style attributes.
type StylePrefs = [(String,String)]

-- | Four modes for highlighting Haskell.
data HsHighlight =
      HsColourInline StylePrefs   -- ^ Use hscolour and inline the styles.
    | HsColourCSS                 -- ^ Use hscolour in conjunction with
                                  --   an external CSS style sheet.
    | HsKate                      -- ^ Use highlighting-kate.
    | HsNoHighlight               -- ^ Do not highlight Haskell.
  deriving (Data,Typeable,Show,Eq)

makePrisms ''HsHighlight

{-

The literate Haskell that Pandoc finds in a file ends up in various
`CodeBlock` elements of the `Pandoc` document.  Other code can also
wind up in `CodeBlock` elements -- normal markdown formatted code.
The `Attr` component has metadata about what's in the code block:

    [haskell]
    type Attr = ( Text,           -- code block identifier
                , [Text]          -- list of code classes
                , [(Text, Text)]  -- name/value pairs
                )

Thanks to some feedback from the Pandoc author, John MacFarlane, I
learned that the CodeBlock *may* contain markers about the kind of
code contained within the block.  LHS (bird-style or LaTex style) will
always have an `Attr` of the form `("",["sourceCode","haskell"],[])`,
and other `CodeBlock` elements are the markdown code blocks *may* have
an identifier, classes, or key/value pairs.  Pandoc captures this info
when the file contains code blocks in the delimited (rather than
indented) format, which allows an optional meta-data specification,
e.g.

~~~~~~~~~~~
~~~~~~~ { .bash }
x=$1
echo $x
~~~~~~~
~~~~~~~~~~~

Although Pandoc supports the above format for marking code blocks (and
annotating the kind of code within the block) I'll also keep my
notation as another option for use with indented blocks, i.e. if you
write:

<pre><code>
    [haskell]
    foo :: String -> String
</code></pre>

it is a Haskell block.  You can also use other annotations, *e.g.*

<pre><code>
    [cpp]
    cout << "Hello World!";
</code></pre>

If highlighting-kate is specified for highlighting Haskell blocks, the
distinction between the literate blocks and the delimited blocks is
lost (this is simply how the Pandoc highlighting module currently
works).

I'll adopt the rule that if you specify a class or classes using
Pandoc's delimited code block syntax, I'll assume that there is no
additional tag within the block in Blog Literately syntax.  I still
need my `unTag` function to parse the code block.


To highlight the syntax using hscolour (which produces HTML), I'm
going to need to transform the `String` from a `CodeBlock` element to
a `String` suitable for the `RawHtml` element (because the hscolour
library transforms Haskell text to HTML). Pandoc strips off the
prepended &gt; characters from the literate Haskell, so I need to put
them back, and also tell hscolour whether the source it is colouring
is literate or not.  The hscolour function looks like:

    [haskell]
    hscolour :: Output      -- ^ Output format.
             -> ColourPrefs -- ^ Colour preferences...
             -> Bool        -- ^ Whether to include anchors.
             -> Bool        -- ^ Whether output document is partial or complete.
             -> String      -- ^ Title for output.
             -> Bool        -- ^ Whether input document is literate haskell
             -> String      -- ^ Haskell source code.
             -> String      -- ^ Coloured Haskell source code.

Since I still don't like the `ICSS` output from hscolour, I'm going to
provide two options for hscolouring to users: one that simply uses
hscolour's `CSS` format, so the user can provide definitions in their
blog's stylesheet to control the rendering, and a post-processing
option to transform the `CSS` class-based rendering into a inline
style based rendering (for people who can't update their stylesheet).
`colourIt` performs the initial transformation:

-}

-- | Use hscolour to syntax highlight some Haskell code.  The first
-- argument indicates whether the code is literate Haskell.
colourIt :: Bool -> Text -> String
colourIt literate srcTxt =
    wrapCode $ hscolour CSS defaultColourPrefs False True "" literate (T.unpack srcTxt')
    where srcTxt' | literate  = litify srcTxt
                  | otherwise = srcTxt
          -- wrap the result in a <pre><code> tag, similar to
          -- highlighting-kate results
          wrapCode s = verbatim $
              (\(Document _ _ e _) -> foldXml filt (CElem e noPos))
                             (xmlParse "colourIt" s)

          attrs = [("class", (("sourceCode haskell")!))]

          filt = mkElemAttr "pre" attrs [mkElemAttr "code" attrs [children]]
                    `when` tag "pre"

-- | Prepend literate Haskell markers to some source code.
litify :: Text -> Text
litify = T.unlines . map (T.append "> ") . T.lines

{-
Hscolour uses HTML `span` elements and CSS classes like 'hs-keyword'
or `hs-keyglyph` to markup Haskell code.  What I want to do is take
each marked `span` element and replace the `class` attribute with an
inline `style` element that has the markup I want for that kind of
source.  Style preferences are specified as a list of name/value
pairs:
-}

-- | A default style that produces something that looks like the
--   source listings on Hackage.
defaultStylePrefs :: StylePrefs
defaultStylePrefs =
  [ ("hs-keyword","color: blue; font-weight: bold;")
  , ("hs-keyglyph","color: red;")
  , ("hs-layout","color: red;")
  , ("hs-comment","color: green;")
  , ("hs-conid", "")
  , ("hs-varid", "")
  , ("hs-conop", "")
  , ("hs-varop", "")
  , ("hs-str", "color: teal;")
  , ("hs-chr", "color: teal;")
  , ("hs-number", "")
  , ("hs-cpp", "")
  , ("hs-selection", "")
  , ("hs-variantselection", "")
  , ("hs-definition", "")
  ]

-- | Read style preferences in from a file using the @Read@ instance
--   for @StylePrefs@, or return the default style if the file name is
--   empty.
getStylePrefs :: Maybe FilePath -> IO StylePrefs
getStylePrefs Nothing      = return defaultStylePrefs
getStylePrefs (Just fname) = liftM read (readFile fname)

-- | Take a @String@ of HTML produced by hscolour, and \"bake\" styles
--   into it by replacing class attributes with appropriate style
--   attributes.
bakeStyles :: StylePrefs -> String -> String
bakeStyles prefs s = verbatim $ filtDoc (xmlParse "bake-input" s)
  where

    -- filter the document (an Hscoloured fragment of Haskell source)
    filtDoc (Document _ _ e _) =  c where
        [c] = filts (CElem e noPos)

    -- the filter is a fold of individual filters for each CSS class
    filts = foldXml $ foldl o keep $ map filt prefs

    -- an individual filter replaces the attributes of a tag with
    -- a style attribute when it has a specific 'class' attribute.
    filt (cls,style) =
        replaceAttrs [("style",style)] `when`
            (attrval $ (N "class", AttValue [Left cls]))

{- Highlighting-Kate uses @\<br/>@ in code blocks to indicate
   newlines.  WordPress (and possibly others) chooses to strip them
   away when found in @\<pre>@ sections of uploaded HTML.  So we
   need to turn them back to newlines.
-}

-- | Replace @\<br/>@ tags with newlines.
replaceBreaks :: String -> String
replaceBreaks s = verbatim $ filtDoc (xmlParse "input" s)
  where
    -- filter the document (a highlighting-kate highlighted fragment of
    -- haskell source)
    filtDoc (Document _ _ e _) = c where
        [c] = filts (CElem e noPos)
    filts = foldXml (literal "\n" `when` tag "br")

{-
Note/todo: the above is a function that could be made better in a
few ways and then factored out into a library.  A way to handle the
above would be to allow the preferences to be specified as an actual
CSS style sheet, which then would be baked into the HTML.  Such a
function could be separately useful, and could be used to 'bake' in
the highlighting-kate styles.
-}

-- | Transform a @CodeBlock@ into a @RawHtml@ block, where
--   the content contains marked up Haskell (possibly with literate
--   markers), or marked up non-Haskell, if highlighting of non-Haskell has
--   been selected.
colouriseCodeBlock :: HsHighlight -> Bool -> Block -> Block
colouriseCodeBlock hsHighlight otherHighlight (CodeBlock attr@(_,classes,_) s)

  | ctag == Just "haskell" || haskell
  = case hsHighlight of
        HsColourInline style ->
            rawHtmlT . bakeStyles style $ colourIt lit src
        HsColourCSS   -> rawHtmlT $ colourIt lit src
        HsNoHighlight -> rawHtml $ simpleHTML hsrc
        HsKate        -> case ctag of
            Nothing -> myHighlightK attr hsrc
            Just t  -> myHighlightK ("", t:classes,[]) hsrc

  | otherHighlight
  = case ctag of
        Nothing -> myHighlightK attr src
        Just t  -> myHighlightK ("",[t],[]) src

  | otherwise
  = rawHtml $ simpleHTML src

  where
    (ctag,src)
        | null classes = unTag s
        | otherwise    = (Nothing, s)
    hsrc
        | lit          = litify src
        | otherwise    = src
    lit          = "sourceCode" `elem` classes
    haskell      = "haskell" `elem` classes
    simpleHTML h = T.append "<pre><code>" (T.append h "</code></pre>")
    myHighlightK attrs h = case highlight formatHtmlBlock attrs h of
        Nothing   -> rawHtml  $ simpleHTML s
        Just html -> rawHtmlT $ replaceBreaks $ renderHtml html
    rawHtmlT = rawHtml . T.pack
    rawHtml  = RawBlock (Format "html")

colouriseCodeBlock _ _ b = b

-- | Perform syntax highlighting on an entire Pandoc document.
colourisePandoc :: HsHighlight -> Bool -> Pandoc -> Pandoc
colourisePandoc hsHighlight otherHighlight (Pandoc m blocks) =
    Pandoc m $ map (colouriseCodeBlock hsHighlight otherHighlight) blocks

--------------------------------------------------
-- highlight function
--------------------------------------------------

-- Copied here from
--
--   https://github.com/jgm/pandoc/blob/8b3a81e4dd8bf46a822980781e28d9777a076c6a/src/Text/Pandoc/Highlighting.hs#L63
--
-- Pandoc 1.11 hid the Text.Pandoc.Highlighting module so we can't
-- import it from there anymore (at least not for the moment).

lcLanguages :: [String]
lcLanguages = map (map toLower) languages

highlight :: (FormatOptions -> [SourceLine] -> a) -- ^ Formatter
          -> Attr    -- ^ Attributes of the CodeBlock
          -> Text    -- ^ Raw contents of the CodeBlock
          -> Maybe a -- ^ Maybe the formatted result
highlight formatter (_, classes, keyvals) rawCode =
  let firstNum = case safeRead (fromMaybe "1" $ lookup "startFrom" keyvals) of
                      Just n  -> n
                      Nothing -> 1
      fmtOpts = defaultFormatOpts{
                  startNumber = firstNum,
                  numberLines = any (`elem`
                        ["number","numberLines", "number-lines"]) classes }
      lcclasses = map (T.unpack . T.toLower) classes
  in case find (`elem` lcLanguages) lcclasses of
            Nothing -> Nothing
            Just language -> Just
                              $ formatter fmtOpts{ codeClasses = [language],
                                                   containerClasses = map T.unpack classes }
                              $ highlightAs language (T.unpack rawCode)
