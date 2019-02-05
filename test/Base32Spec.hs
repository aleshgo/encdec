{-# LANGUAGE DataKinds #-}

module Base32Spec where

import Encdec.Decoder as Decoder
import Encdec.Types
import Test.Hspec
import Data.ByteString (ByteString)

spec :: Spec
spec = do
  describe "decode" $ do
    it "conform RFC examples" $ do
      Decoder.decode (Encoded "" :: Encoded 'Base32) `shouldBe` (Ok "" :: Result ByteString)
      Decoder.decode (Encoded "MY======" :: Encoded 'Base32) `shouldBe` (Ok "f" :: Result ByteString)
      Decoder.decode (Encoded "MZXQ====" :: Encoded 'Base32) `shouldBe` (Ok "fo" :: Result ByteString)
      Decoder.decode (Encoded "MZXW6===" :: Encoded 'Base32) `shouldBe` (Ok "foo" :: Result ByteString)
      Decoder.decode (Encoded "MZXW6YQ=" :: Encoded 'Base32) `shouldBe` (Ok "foob" :: Result ByteString)
      Decoder.decode (Encoded "MZXW6YTB" :: Encoded 'Base32) `shouldBe` (Ok "fooba" :: Result ByteString)
      Decoder.decode (Encoded "MZXW6YTBOI======" :: Encoded 'Base32) `shouldBe` (Ok "foobar" :: Result ByteString)
