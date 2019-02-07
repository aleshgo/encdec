{-# LANGUAGE DataKinds #-}

module Base64Spec where

import Encdec.Decoder as Decoder
import Encdec.Encoder as Encoder
import Encdec.Utils as Utils
import Encdec.Types
import Test.Hspec
import Data.ByteString (ByteString)

spec :: Spec
spec = do
  describe "decode" $ do
    it "conform RFC examples" $ do
      Decoder.decode (Encoded "" :: Encoded 'Base64) `shouldBe` (Ok "" :: Result ByteString)
      Decoder.decode (Encoded "Zg==" :: Encoded 'Base64) `shouldBe` (Ok "f" :: Result ByteString)
      Decoder.decode (Encoded "Zm8=" :: Encoded 'Base64) `shouldBe` (Ok "fo" :: Result ByteString)
      Decoder.decode (Encoded "Zm9v" :: Encoded 'Base64) `shouldBe` (Ok "foo" :: Result ByteString)
      Decoder.decode (Encoded "Zm9vYg==" :: Encoded 'Base64) `shouldBe` (Ok "foob" :: Result ByteString)
      Decoder.decode (Encoded "Zm9vYmE=" :: Encoded 'Base64) `shouldBe` (Ok "fooba" :: Result ByteString)
      Decoder.decode (Encoded "Zm9vYmFy" :: Encoded 'Base64) `shouldBe` (Ok "foobar" :: Result ByteString)

  describe "encode" $ do
    it "conform RFC examples" $ do
      Encoder.encode ("" :: ByteString) `shouldBe` (Encoded "" :: Encoded 'Base64)
      Encoder.encode ("f" :: ByteString) `shouldBe` (Encoded "Zg==" :: Encoded 'Base64)
      Encoder.encode ("fo" :: ByteString) `shouldBe` (Encoded "Zm8=" :: Encoded 'Base64)
      Encoder.encode ("foo" :: ByteString) `shouldBe` (Encoded "Zm9v" :: Encoded 'Base64)
      Encoder.encode ("foob" :: ByteString) `shouldBe` (Encoded "Zm9vYg==" :: Encoded 'Base64)
      Encoder.encode ("fooba" :: ByteString) `shouldBe` (Encoded "Zm9vYmE=" :: Encoded 'Base64)
      Encoder.encode ("foobar" :: ByteString) `shouldBe` (Encoded "Zm9vYmFy" :: Encoded 'Base64)

  describe "pad" $ do
    it "conform examples" $ do
      Utils.pad (Encoded "" :: Encoded 'Base64) `shouldBe` (Encoded "" :: Encoded 'Base64)
      Utils.pad (Encoded "Zg" :: Encoded 'Base64) `shouldBe` (Encoded "Zg==" :: Encoded 'Base64)
      Utils.pad (Encoded "Zm8" :: Encoded 'Base64) `shouldBe` (Encoded "Zm8=" :: Encoded 'Base64)
      Utils.pad (Encoded "Zm9v" :: Encoded 'Base64) `shouldBe` (Encoded "Zm9v" :: Encoded 'Base64)
      Utils.pad (Encoded "Zm9vYg" :: Encoded 'Base64) `shouldBe` (Encoded "Zm9vYg==" :: Encoded 'Base64)
      Utils.pad (Encoded "Zm9vYmE" :: Encoded 'Base64) `shouldBe` (Encoded "Zm9vYmE=" :: Encoded 'Base64)
      Utils.pad (Encoded "Zm9vYmFy" :: Encoded 'Base64) `shouldBe` (Encoded "Zm9vYmFy" :: Encoded 'Base64)

  describe "unpad" $ do
    it "conform examples" $ do
      Utils.unpad (Encoded "" :: Encoded 'Base64) `shouldBe` (Encoded "" :: Encoded 'Base64)
      Utils.unpad (Encoded "Zg==" :: Encoded 'Base64) `shouldBe` (Encoded "Zg" :: Encoded 'Base64)
      Utils.unpad (Encoded "Zm8=" :: Encoded 'Base64) `shouldBe` (Encoded "Zm8" :: Encoded 'Base64)
      Utils.unpad (Encoded "Zm9v" :: Encoded 'Base64) `shouldBe` (Encoded "Zm9v" :: Encoded 'Base64)
      Utils.unpad (Encoded "Zm9vYg==" :: Encoded 'Base64) `shouldBe` (Encoded "Zm9vYg" :: Encoded 'Base64)
      Utils.unpad (Encoded "Zm9vYmE=" :: Encoded 'Base64) `shouldBe` (Encoded "Zm9vYmE" :: Encoded 'Base64)
      Utils.unpad (Encoded "Zm9vYmFy" :: Encoded 'Base64) `shouldBe` (Encoded "Zm9vYmFy" :: Encoded 'Base64)
