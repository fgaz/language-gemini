{-# LANGUAGE OverloadedStrings #-}
module Language.Gemini (
-- * Gemini documents
  GeminiDocument
, GeminiLine(..)
-- * Decoding
, decodeGemini
-- * Encoding
, encodeGemini
) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Data.Char (isSpace)
import Data.Int (Int64)

-- Gemini documents
----------------------------

type GeminiDocument = [GeminiLine]

data GeminiLine = LText Text
                | LLink Text (Maybe Text)
                | LPre [Text] --not really a line...
                | LH1 Text
                | LH2 Text
                | LH3 Text
                | LItem Text
  deriving (Show, Read, Eq)

-- Decoding
----------------------------

decodeGemini :: Bool -- ^ Whether to allow unix-style line endings (\n)
             -> Text -- ^ Text to parse
             -> GeminiDocument
-- gemini is really simple, so we do not even use a parsing library
decodeGemini allowUnixStyle = go . (if allowUnixStyle then concatMap T.lines else id)
                                 . T.splitOn "\CR\LF"
  where
    go [] = []
    go (l:ls) | isPreToggle l = let (pres, rest) = break isPreToggle ls
                                 in LPre pres : go (drop 1 rest)
              | "=>" `T.isPrefixOf` l = parseLink l : go ls
              | "###" `T.isPrefixOf` l = LH3 (dropPrefix 3 l) : go ls
              | "##" `T.isPrefixOf` l = LH2 (dropPrefix 2 l) : go ls
              | "#" `T.isPrefixOf` l = LH1 (dropPrefix 1 l) : go ls
              | "*" `T.isPrefixOf` l = LItem (dropPrefix 1 l) : go ls
              | otherwise = LText l : go ls

isPreToggle :: Text -> Bool
isPreToggle = T.isPrefixOf "```"

dropPrefix :: Int64 -> Text -> Text
dropPrefix n = T.stripStart . T.drop n

parseLink :: Text -> GeminiLine
parseLink txt = LLink link $ if T.null desc' then Nothing else Just desc'
  where
    (link, desc) = T.break isSpace $ T.stripStart txt
    desc' = T.stripStart desc

-- Encoding
----------------------------

encodeGemini :: GeminiDocument -> Text
encodeGemini = T.intercalate "\CR\LF" . fmap encodeLine

encodeLine :: GeminiLine -> Text
encodeLine (LText t) = escapeLText t
encodeLine (LLink l desc) = "=> " <> escapeLink l <> " " <> desc'
  where desc' = maybe T.empty escapeNewlines desc
encodeLine (LPre ls) = T.intercalate "\CR\LF" $
  "```" : fmap escapeLPre ls <> ["```"]
encodeLine (LH1 t) = "# " <> escapeNewlines t
encodeLine (LH2 t) = "## " <> escapeNewlines t
encodeLine (LH3 t) = "### " <> escapeNewlines t
encodeLine (LItem t) = "* " <> escapeNewlines t

--- TODO ask about actual escaping rules instead of just using "\\" and stripping newlines

escapeLPre :: Text -> Text
escapeLPre = escapePrePrefix . escapeNewlines

escapeLText :: Text -> Text
escapeLText = escapeAnyPrefix . escapeNewlines

escapeLink :: Text -> Text
-- Ideally spaces should be urlencoded but nonmalicious agents wouldn't put
-- whitespace in a link anyway.
escapeLink = T.map $ \c -> if isSpace c then '+' else c

escapeNewlines :: Text -> Text
escapeNewlines = T.map crlfToSpace
  where
    crlfToSpace '\CR' = ' '
    crlfToSpace '\LF' = ' '
    crlfToSpace c     = c

escapePrePrefix :: Text -> Text
escapePrePrefix t | "```" `T.isPrefixOf` t = T.cons '\\' t
                  | otherwise                     = t

escapeAnyPrefix :: Text -> Text
escapeAnyPrefix t | reservedPrefix t = T.cons '\\' t
                  | otherwise        = t

reservedPrefix :: Text -> Bool
reservedPrefix t = any (`T.isPrefixOf` t)
  [ "=>"
  , "```"
  , "#"
  , "*"
  ]

