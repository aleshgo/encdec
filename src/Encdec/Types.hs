{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Encdec.Types where

import Data.ByteString (ByteString)

data Encoding
  = Base16
  | Base32
  | Base64
  | Hex

newtype Encoded (a :: Encoding) =
  Encoded ByteString
  deriving (Eq, Show)

data Result a
  = Err ByteString
  | Ok a
  deriving (Eq, Show)

leftToErr :: ByteString -> a -> Result b
leftToErr = const . Err

infixr 0 <|
infixl 0 |>

(<|) = ($)
(|>) = flip ($)
