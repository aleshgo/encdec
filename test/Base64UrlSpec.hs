{-# LANGUAGE DataKinds #-}

module Base64UrlSpec where

import Encdec.Decoder as Decoder
import Encdec.Encoder as Encoder
import Encdec.Types
import Test.Hspec
import Data.ByteString (ByteString)

spec :: Spec
spec = do
  describe "decode" $ do
    it "conform RFC examples" $ do
      Decoder.decode (Encoded "" :: Encoded 'Base64Url) `shouldBe` (Ok "" :: Result ByteString)
      Decoder.decode (Encoded "Zg==" :: Encoded 'Base64Url) `shouldBe` (Ok "f" :: Result ByteString)
      Decoder.decode (Encoded "Zm8=" :: Encoded 'Base64Url) `shouldBe` (Ok "fo" :: Result ByteString)
      Decoder.decode (Encoded "Zm9v" :: Encoded 'Base64Url) `shouldBe` (Ok "foo" :: Result ByteString)
      Decoder.decode (Encoded "Zm9vYg==" :: Encoded 'Base64Url) `shouldBe` (Ok "foob" :: Result ByteString)
      Decoder.decode (Encoded "Zm9vYmE=" :: Encoded 'Base64Url) `shouldBe` (Ok "fooba" :: Result ByteString)
      Decoder.decode (Encoded "Zm9vYmFy" :: Encoded 'Base64Url) `shouldBe` (Ok "foobar" :: Result ByteString)
      Decoder.decode (Encoded "_--_" :: Encoded 'Base64Url) `shouldBe` (Ok "\255\239\191" :: Result ByteString)

  describe "encode" $ do
    it "conform RFC examples" $ do
      Encoder.encode ("" :: ByteString) `shouldBe` (Encoded "" :: Encoded 'Base64Url)
      Encoder.encode ("f" :: ByteString) `shouldBe` (Encoded "Zg==" :: Encoded 'Base64Url)
      Encoder.encode ("fo" :: ByteString) `shouldBe` (Encoded "Zm8=" :: Encoded 'Base64Url)
      Encoder.encode ("foo" :: ByteString) `shouldBe` (Encoded "Zm9v" :: Encoded 'Base64Url)
      Encoder.encode ("foob" :: ByteString) `shouldBe` (Encoded "Zm9vYg==" :: Encoded 'Base64Url)
      Encoder.encode ("fooba" :: ByteString) `shouldBe` (Encoded "Zm9vYmE=" :: Encoded 'Base64Url)
      Encoder.encode ("foobar" :: ByteString) `shouldBe` (Encoded "Zm9vYmFy" :: Encoded 'Base64Url)
      Encoder.encode ("\255\239\191" :: ByteString) `shouldBe` (Encoded "_--_" :: Encoded 'Base64Url)
