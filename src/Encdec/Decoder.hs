{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Encdec.Decoder
  ( decode
  ) where

import qualified Codec.Binary.Base32 as Base32
import Control.Arrow
import Data.ByteString (ByteString)
import Data.Hex as Hex
import Encdec.Types (Encoded(..), Encoding(Base32, Hex))

class Decoder a where
  decode :: a -> Either ByteString ByteString

instance Decoder (Encoded 'Base32) where
  decode (Encoded a) =
    left (const "Decoding from Base32 error") $ Base32.decode a

instance Decoder (Encoded 'Hex) where
  decode (Encoded a) = Hex.unhex a
