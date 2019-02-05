{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Encdec.Decoder
  ( decode
  ) where

import qualified Codec.Binary.Base32 as Base32
import Control.Arrow
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Hex as Hex
import Encdec.Types (Encoded(..), Encoding(..))
import qualified Numeric as N

class Decoder a where
  decode :: a -> Either ByteString ByteString

instance Decoder (Encoded ByteString 'Base32) where
  decode (Encoded a) =
    left (const "Decoding from Base32 error") $ Base32.decode a

instance Decoder (Encoded ByteString 'Hex) where
  decode (Encoded a) = Hex.unhex a

instance Decoder (Encoded Int 'Utc) where
  decode (Encoded a) = 
    Right (pack $ N.showHex (toInteger a) "")
