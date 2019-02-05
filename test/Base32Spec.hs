{-# LANGUAGE DataKinds #-}

module Base32Spec where

import Encdec.Decoder as Decoder
import Encdec.Types (Encoded(..), Encoding(Base32, Hex))
import Test.Hspec

spec :: Spec
spec = do
  describe "decode" $ do
    it "conform RFC examples" $ do
      Decoder.decode (Encoded "" :: Encoded 'Base32) `shouldBe` Right ""
      Decoder.decode (Encoded "MY======" :: Encoded 'Base32) `shouldBe` Right "f"
      Decoder.decode (Encoded "MZXQ====" :: Encoded 'Base32) `shouldBe` Right "fo"
