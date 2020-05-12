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
encodeGemini = T.intercalate (T.pack "\CR\LF") . fmap encodeLine

encodeLine :: GeminiLine -> Text
encodeLine (LText t) = escapeText t
encodeLine (LLink l desc) = T.pack "=> " <> l <> T.pack " " <> desc'
  where desc' = maybe T.empty (T.pack "" <>) desc
encodeLine (LPre ls) = T.intercalate (T.pack "\CR\LF") $
  T.pack "```" : fmap escapePre ls <> [T.pack "```"]
encodeLine (LH1 t) = T.pack "# " <> t
encodeLine (LH2 t) = T.pack "## " <> t
encodeLine (LH3 t) = T.pack "### " <> t
encodeLine (LItem t) = T.pack "* " <> t

--- TODO ask about actual escaping rules

escapeText :: Text -> Text
escapeText t | reservedPrefix t = T.cons '\\' t
             | otherwise        = t

escapePre :: Text -> Text
escapePre t | T.pack "```" `T.isPrefixOf` t = T.cons '\\' t
            | otherwise                     = t

reservedPrefix :: Text -> Bool
reservedPrefix t = any (`T.isPrefixOf` t) $ T.pack <$>
  [ "=>"
  , "```"
  , "#"
  , "*"
  ]

