{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Encdec.Types (Decoded(..), Encoding(..))  where

import Data.ByteString (ByteString)

data Encoding = Base16 | Base32 | Hex

newtype Decoded (a :: Encoding) = Decoded ByteString