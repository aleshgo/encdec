{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Encdec.Types (Encoded(..), Encoding(..))  where

import Data.ByteString (ByteString)

data Encoding = Base16 | Base32 | Hex

newtype Encoded (a :: Encoding) = Encoded ByteString
