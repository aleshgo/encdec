{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Encdec.Decoder
  ( decode
  ) where

import qualified Codec.Binary.Base64Url as Base64Url
import qualified Codec.Binary.Base64 as Base64
import qualified Codec.Binary.Base32 as Base32
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Hex as Hex
import Encdec.Types
import qualified Numeric as Num
import Safe (headMay)

class Decoder a b where
  decode :: a -> Result b

instance Decoder (Encoded 'Base64Url) ByteString where
  decode (Encoded a) =
    Base64Url.decode a
      |> either (leftToErr "Decoding from Base64Url to ByteString error") Ok

instance Decoder (Encoded 'Base64) ByteString where
  decode (Encoded a) =
    Base64.decode a
      |> either (leftToErr "Decoding from Base64 to ByteString error") Ok

instance Decoder (Encoded 'Base32) ByteString where
  decode (Encoded a) =
    Base32.decode a
      |> either (leftToErr "Decoding from Base32 to ByteString error") Ok

instance Decoder (Encoded 'Hex) ByteString where
  decode (Encoded a) =
    Hex.unhex a
      |> either (leftToErr "Decoding from Hex to ByteString error") Ok

instance Decoder (Encoded 'Hex) Int where
  decode (Encoded a) =
    unpack a
      |> Num.readHex
      |> headMay
      |> maybe (Err "Decoding from Hex to Int error") (Ok . fst)
