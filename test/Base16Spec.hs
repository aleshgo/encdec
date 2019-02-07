{-# LANGUAGE DataKinds #-}

module Base16Spec where

import Encdec.Decoder as Decoder
import Encdec.Encoder as Encoder
import Encdec.Types
import Test.Hspec
import Data.ByteString (ByteString)

spec :: Spec
spec = do
  describe "decode" $ do
    it "conform RFC examples" $ do
      Decoder.decode (Encoded "" :: Encoded 'Base16) `shouldBe` (Ok "" :: Result ByteString)
      Decoder.decode (Encoded "66" :: Encoded 'Base16) `shouldBe` (Ok "f" :: Result ByteString)
      Decoder.decode (Encoded "666F" :: Encoded 'Base16) `shouldBe` (Ok "fo" :: Result ByteString)
      Decoder.decode (Encoded "666F6F" :: Encoded 'Base16) `shouldBe` (Ok "foo" :: Result ByteString)
      Decoder.decode (Encoded "666F6F62" :: Encoded 'Base16) `shouldBe` (Ok "foob" :: Result ByteString)
      Decoder.decode (Encoded "666F6F6261" :: Encoded 'Base16) `shouldBe` (Ok "fooba" :: Result ByteString)
      Decoder.decode (Encoded "666F6F626172" :: Encoded 'Base16) `shouldBe` (Ok "foobar" :: Result ByteString)

  describe "encode" $ do
    it "conform RFC examples" $ do
      Encoder.encode ("" :: ByteString) `shouldBe` (Encoded "" :: Encoded 'Base16)
      Encoder.encode ("f" :: ByteString) `shouldBe` (Encoded "66" :: Encoded 'Base16)
      Encoder.encode ("fo" :: ByteString) `shouldBe` (Encoded "666F" :: Encoded 'Base16)
      Encoder.encode ("foo" :: ByteString) `shouldBe` (Encoded "666F6F" :: Encoded 'Base16)
      Encoder.encode ("foob" :: ByteString) `shouldBe` (Encoded "666F6F62" :: Encoded 'Base16)
      Encoder.encode ("fooba" :: ByteString) `shouldBe` (Encoded "666F6F6261" :: Encoded 'Base16)
      Encoder.encode ("foobar" :: ByteString) `shouldBe` (Encoded "666F6F626172" :: Encoded 'Base16)
