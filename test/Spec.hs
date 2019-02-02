{-# LANGUAGE DataKinds #-}
import Test.Hspec
import Encdec.Decoder as Decoder
import Encdec.Types (Decoded(..), Encoding(Base32, Hex)) 

main :: IO ()
main = do
  putStrLn "Test suite not yet implemented"
  hspec spec

spec :: Spec
spec = do
  describe "decode" $ do
    it "conform RFC examples" $ do
      Decoder.decode (Decoded "" :: Decoded 'Base32) `shouldBe` Right ""
      Decoder.decode (Decoded "MY======" :: Decoded 'Base32) `shouldBe` Right "f"
      Decoder.decode (Decoded "MZXQ====" :: Decoded 'Base32) `shouldBe` Right "fo"
      Decoder.decode (Decoded "66" :: Decoded 'Hex) `shouldBe` Right "f"