{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Encdec.Encoder
  ( encode
  ) where

import qualified Codec.Binary.Base64 as Base64
import qualified Codec.Binary.Base32 as Base32
import qualified Numeric as Num
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Hex as Hex
import Encdec.Types

class Encoder a b where
  encode :: a -> b

instance Encoder ByteString (Encoded 'Base64) where
  encode = Encoded . Base64.encode

instance Encoder ByteString (Encoded 'Base32) where
  encode = Encoded . Base32.encode

instance Encoder ByteString (Encoded 'Hex) where
  encode = Encoded . Hex.hex

instance Encoder Int (Encoded 'Hex) where
  encode = Encoded . pack . flip Num.showHex ""
