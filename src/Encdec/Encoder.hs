{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Encdec.Encoder
  ( encode
  ) where

import qualified Codec.Binary.Base32 as Base32
import Data.ByteString (ByteString)
import Data.Hex as Hex
import Encdec.Types

class Encoder a b where
  encode :: a -> b

instance Encoder ByteString (Encoded 'Base32) where
  encode = Encoded . Base32.encode

instance Encoder ByteString (Encoded 'Hex) where
  encode = Encoded . Hex.hex
