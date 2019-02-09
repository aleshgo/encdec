module Base32Spec where

import Encdec.Decoder as Decoder
import Encdec.Encoder as Encoder
import Encdec.Utils as Utils
import Encdec.Types
import Encdec.Encoding
import Test.Hspec
import Data.ByteString (ByteString)

spec :: Spec
spec = do
  describe "decode" $ do
    it "conform RFC examples" $ do
      Decoder.decode (Encoded "" :: Encoded Base32) `shouldBe` (Ok "" :: Result ByteString)
      Decoder.decode (Encoded "MY======" :: Encoded Base32) `shouldBe` (Ok "f" :: Result ByteString)
      Decoder.decode (Encoded "MZXQ====" :: Encoded Base32) `shouldBe` (Ok "fo" :: Result ByteString)
      Decoder.decode (Encoded "MZXW6===" :: Encoded Base32) `shouldBe` (Ok "foo" :: Result ByteString)
      Decoder.decode (Encoded "MZXW6YQ=" :: Encoded Base32) `shouldBe` (Ok "foob" :: Result ByteString)
      Decoder.decode (Encoded "MZXW6YTB" :: Encoded Base32) `shouldBe` (Ok "fooba" :: Result ByteString)
      Decoder.decode (Encoded "MZXW6YTBOI======" :: Encoded Base32) `shouldBe` (Ok "foobar" :: Result ByteString)

  describe "encode" $ do
    it "conform RFC examples" $ do
      Encoder.encode ("" :: ByteString) `shouldBe` (Encoded "" :: Encoded Base32)
      Encoder.encode ("f" :: ByteString) `shouldBe` (Encoded "MY======" :: Encoded Base32)
      Encoder.encode ("fo" :: ByteString) `shouldBe` (Encoded "MZXQ====" :: Encoded Base32)
      Encoder.encode ("foo" :: ByteString) `shouldBe` (Encoded "MZXW6===" :: Encoded Base32)
      Encoder.encode ("foob" :: ByteString) `shouldBe` (Encoded "MZXW6YQ=" :: Encoded Base32)
      Encoder.encode ("fooba" :: ByteString) `shouldBe` (Encoded "MZXW6YTB" :: Encoded Base32)
      Encoder.encode ("foobar" :: ByteString) `shouldBe` (Encoded "MZXW6YTBOI======" :: Encoded Base32)

  describe "pad" $ do
    it "conform examples" $ do
      Utils.pad (Encoded "" :: Encoded Base32) `shouldBe` (Encoded "" :: Encoded Base32)
      Utils.pad (Encoded "MY" :: Encoded Base32) `shouldBe` (Encoded "MY======" :: Encoded Base32)
      Utils.pad (Encoded "MZXQ" :: Encoded Base32) `shouldBe` (Encoded "MZXQ====" :: Encoded Base32)
      Utils.pad (Encoded "MZXW6" :: Encoded Base32) `shouldBe` (Encoded "MZXW6===" :: Encoded Base32)
      Utils.pad (Encoded "MZXW6YQ" :: Encoded Base32) `shouldBe` (Encoded "MZXW6YQ=" :: Encoded Base32)
      Utils.pad (Encoded "MZXW6YTB" :: Encoded Base32) `shouldBe` (Encoded "MZXW6YTB" :: Encoded Base32)
      Utils.pad (Encoded "MZXW6YTBOI" :: Encoded Base32) `shouldBe` (Encoded "MZXW6YTBOI======" :: Encoded Base32)

  describe "unpad" $ do
    it "conform examples" $ do
      Utils.unpad (Encoded "" :: Encoded Base32) `shouldBe` (Encoded "" :: Encoded Base32)
      Utils.unpad (Encoded "MY======" :: Encoded Base32) `shouldBe` (Encoded "MY" :: Encoded Base32)
      Utils.unpad (Encoded "MZXQ====" :: Encoded Base32) `shouldBe` (Encoded "MZXQ" :: Encoded Base32)
      Utils.unpad (Encoded "MZXW6===" :: Encoded Base32) `shouldBe` (Encoded "MZXW6" :: Encoded Base32)
      Utils.unpad (Encoded "MZXW6YQ=" :: Encoded Base32) `shouldBe` (Encoded "MZXW6YQ" :: Encoded Base32)
      Utils.unpad (Encoded "MZXW6YTB" :: Encoded Base32) `shouldBe` (Encoded "MZXW6YTB" :: Encoded Base32)
      Utils.unpad (Encoded "MZXW6YTBOI======" :: Encoded Base32) `shouldBe` (Encoded "MZXW6YTBOI" :: Encoded Base32)
