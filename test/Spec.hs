{-# LANGUAGE DataKinds #-}
import Test.Hspec
import Encdec.Decoder as Decoder
import Encdec.Types (Encoded(..), Encoding(Base32, Hex))

main :: IO ()
main = do
  putStrLn "Test suite not yet implemented"
  hspec spec

spec :: Spec
spec = do
  describe "decode" $ do
    it "conform RFC examples" $ do
      Decoder.decode (Encoded "" :: Encoded 'Base32) `shouldBe` Right ""
      Decoder.decode (Encoded "MY======" :: Encoded 'Base32) `shouldBe` Right "f"
      Decoder.decode (Encoded "MZXQ====" :: Encoded 'Base32) `shouldBe` Right "fo"
      Decoder.decode (Encoded "66" :: Encoded 'Hex) `shouldBe` Right "f"
