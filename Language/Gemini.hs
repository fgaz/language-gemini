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
import Data.Maybe (fromMaybe)

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

decodeGemini :: Text -> GeminiDocument
decodeGemini = error "Gemini decoding not implemented" --TODO (also lenient (LF-only) variant)

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

