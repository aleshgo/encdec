{-# LANGUAGE DataKinds #-}

import Encdec.Decoder as Decoder
import Encdec.Types (Encoded(..), Encoding(..))
import Data.ByteString (ByteString)
import Test.Hspec

main :: IO ()
main = do
  putStrLn "Test suite not yet implemented"
  hspec spec

spec :: Spec
spec = do
  describe "decode" $ do
    it "conform RFC examples" $ do
      Decoder.decode (Encoded "" :: Encoded ByteString 'Base32) `shouldBe` Right ""
      Decoder.decode (Encoded "MY======" :: Encoded ByteString 'Base32) `shouldBe`
        Right "f"
      Decoder.decode (Encoded "MZXQ====" :: Encoded ByteString 'Base32) `shouldBe`
        Right "fo"
      Decoder.decode (Encoded "66" :: Encoded ByteString 'Hex) `shouldBe` Right "f"

    it "utc to hex" $ do
      Decoder.decode (Encoded 11 :: Encoded Int 'Utc) `shouldBe` Right "b"