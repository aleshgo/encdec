{-# LANGUAGE DataKinds #-}

module HexSpec where

import Encdec.Decoder as Decoder
import Encdec.Types
import Data.ByteString (ByteString)
import Test.Hspec

spec :: Spec
spec = do
  describe "decode" $ do
    it "conform examples" $ do
      Decoder.decode (Encoded "66" :: Encoded 'Hex) `shouldBe` (Ok "f" :: Result ByteString)
