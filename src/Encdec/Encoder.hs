{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Encdec.Encoder
  ( encode
  ) where

import qualified Codec.Binary.Base64Url as Base64Url
import qualified Codec.Binary.Base64 as Base64
import qualified Codec.Binary.Base32 as Base32
import qualified Codec.Binary.Base16 as Base16
import qualified Numeric as Num
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Hex as Hex
import Encdec.Types

class Encoder a b where
  encode :: a -> b

instance Encoder ByteString (Encoded 'Base64Url) where
  encode = Encoded . Base64Url.encode

instance Encoder ByteString (Encoded 'Base64) where
  encode = Encoded . Base64.encode

instance Encoder ByteString (Encoded 'Base32) where
  encode = Encoded . Base32.encode

instance Encoder ByteString (Encoded 'Base16) where
  encode = Encoded . Base16.encode

instance Encoder Int (Encoded 'Hex) where
  encode = Encoded . pack . flip Num.showHex ""
