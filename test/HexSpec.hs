module HexSpec where

import HuskPrelude
import Data.Result
import Test.Hspec

import Encdec.Decoder as Decoder
import Encdec.Encoder as Encoder
import Encdec.Types
import Encdec.Encoding

spec :: Spec
spec = do
  describe "decode" $ do
    it "conform examples" $ do
      Decoder.decode (Encoded "f" :: Encoded Hex) `shouldBe` (Ok 15 :: Result Int)

  describe "encode" $ do
    it "conform examples" $ do
      Encoder.encode (15 :: Int) `shouldBe` (Encoded "f" :: Encoded Hex)
