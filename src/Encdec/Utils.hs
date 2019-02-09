module Encdec.Utils where

import HuskPrelude
import qualified Data.ByteString as BS

import Encdec.Encoded
import Encdec.Encoding


-- | Classes

class Pad a where
  pad :: Encoded a -> Encoded a

class Unpad a where
  unpad :: Encoded a -> Encoded a


pad' n (Encoded a) =
  a <> BS.replicate ((n - BS.length a `mod` n) `mod` n) 61 |> Encoded

unpad' (Encoded a) = a |> BS.reverse . BS.dropWhile (== 61) . BS.reverse |> Encoded


-- | Unpad instances

instance Pad Base64Url where
  pad = pad' 4

instance Pad Base64 where
  pad = pad' 4

instance Pad Base32 where
  pad = pad' 8


-- | Unpad instances

instance Unpad Base64Url where
  unpad = unpad'

instance Unpad Base64 where
  unpad = unpad'

instance Unpad Base32 where
  unpad = unpad'
