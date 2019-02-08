module HexSpec where

import Encdec.Decoder as Decoder
import Encdec.Encoder as Encoder
import Encdec.Types
import Data.ByteString (ByteString)
import Test.Hspec

spec :: Spec
spec = do
  describe "decode" $ do
    it "conform examples" $ do
      Decoder.decode (Encoded "f" :: Encoded Hex) `shouldBe` (Ok 15 :: Result Int)

  describe "encode" $ do
    it "conform examples" $ do
      Encoder.encode (15 :: Int) `shouldBe` (Encoded "f" :: Encoded Hex)
