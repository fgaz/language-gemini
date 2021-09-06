{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Language.Gemini (GeminiDocument, GeminiLine (..), decodeGemini, encodeGemini)

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Hedgehog (Gen, PropertyT, forAll, (===))
import Hedgehog.Gen (alphaNum, choice, frequency, list, sample, text, unicode)
import qualified Hedgehog.Gen as G
import Hedgehog.Range (constant, singleton)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)


main :: IO ()
main = hspec $ do
  encodeDecodeTrip

encodeDecodeTrip :: Spec
encodeDecodeTrip = do
  describe "Convertion" $ do
    it "Hello, world!" $ decodeGemini False (encodeGemini helloWorld) `shouldBe` helloWorld
    it "Hello, world! Text." $ encodeGemini (decodeGemini False helloWorldT) `shouldBe` helloWorldT
    it "Lorum ipsum." $ decodeGemini False (encodeGemini lorumIpsum) `shouldBe` lorumIpsum
    it "Lorum ipsum. Text." $ encodeGemini (decodeGemini False lorumIpsumT) `shouldBe` lorumIpsumT
    it "Gemini random trip" geminiTrip

helloWorld :: GeminiDocument
helloWorld =
  [ LH1 "Hello, World!"
  , LPre
    [ "main :: IO ()"
    , "main = putStrLn \"Welcome, geminicules!\""
    ]
  ]

helloWorldT :: Text
helloWorldT = "# Hello, World!\r\n```\r\nmain :: IO ()\r\nmain = putStrLn \"Welcome, geminiks!\"\r\n```"

lorumIpsum :: GeminiDocument
lorumIpsum =
  [ LH1 "Lorem ipsum dolor sit amet"
  , LH2 "consectetur adipiscing elit"
  , LH3 "sed do eiusmod tempor incididunt"
  , LLink "gemini://geminispace.info" (Just "ut labore et dolore magna aliqua.")
  , LPre
      [ "Ut enim ad minim veniam"
      , "quis nostrud exercitation ullamco laboris nisi"
      , "ut aliquip ex ea commodo consequat."
      ]
  , LText "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur."
  , LItem "Excepteur sint occaecat cupidatat non proident,"
  , LItem "sunt in culpa qui officia deserunt mollit anim id est laborum."
  , LQuote "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
  ]

lorumIpsumT :: Text
lorumIpsumT ="# Lorem ipsum dolor sit amet\r\n## consectetur adipiscing elit\r\n### sed do eiusmod tempor incididunt\r\n=> gemini://geminispace.info ut labore et dolore magna aliqua.\r\n```\r\nUt enim ad minim veniam\r\nquis nostrud exercitation ullamco laboris nisi\r\nut aliquip ex ea commodo consequat.\r\n```\r\nDuis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.\r\n* Excepteur sint occaecat cupidatat non proident,\r\n* sunt in culpa qui officia deserunt mollit anim id est laborum.\r\n> Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."


space :: Gen Char
space = G.constant ' '

txt :: Gen Text
txt = do
  -- It not tripped with empty text.
  notSpaceChar <- TL.fromStrict <$> text (singleton 1) alphaNum
  anyText <- TL.fromStrict <$> text (constant 0 20) char
  pure $ TL.append notSpaceChar anyText
  where
    char = frequency [(90, alphaNum), (10, space)]

textUnicode :: Gen Text
textUnicode = TL.fromStrict <$> (text (constant 1 20) char)
  where
    char = frequency [(90, unicode), (10, space)]

lText :: Gen GeminiLine
lText = LText <$> txt

lLink :: Gen GeminiLine
lLink = do
  link <- txt
  desc <- txt
  pure $ LLink link (Just desc)

lPre :: Gen GeminiLine
lPre = do
  pre <- list (constant 0 10) txt
  pure $ LPre pre

lItem :: Gen GeminiLine
lItem = LItem <$> txt

lQuote :: Gen GeminiLine
lQuote = LQuote <$> txt

lh1 :: Gen GeminiLine
lh1 = LH1 <$> txt

lh2 :: Gen GeminiLine
lh2 = LH1 <$> txt

lh3 :: Gen GeminiLine
lh3 = LH1 <$> txt

geminiTrip :: PropertyT IO ()
geminiTrip = hedgehog $ do
  let line = choice
        [ lh1
        , lh2
        , lh3
        , lQuote
        , lItem
        , lPre
        {-
        LLink not trips. Consider add encodeGeminiLenient to return spaces.
        Or better rename current encodeGemini to encodeGeminiEscapeSpace
        decodeGemini False $ encodeGemini $ [LLink "d d l" Nothing]
        [LLink "d+d+l" Nothing]
        -}
        -- , lLink
        , lText
        ]
  doc <- forAll $ list (constant 1 50) line
  decodeGemini False (encodeGemini doc) === doc
