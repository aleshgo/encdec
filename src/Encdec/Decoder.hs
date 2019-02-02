{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Encdec.Decoder (decode) where

import Data.ByteString (ByteString)
import Encdec.Types (Decoded(..), Encoding(Hex, Base32)) 
import qualified Codec.Binary.Base32 as Base32
import Control.Arrow
import Data.Hex as Hex 

class Decoder a where
  decode :: a -> Either ByteString ByteString

instance Decoder (Decoded 'Base32) where
  decode (Decoded a) = left (const "Decoding from Base32 error") $ Base32.decode a

instance Decoder (Decoded 'Hex) where
  decode (Decoded a) = Hex.unhex a 