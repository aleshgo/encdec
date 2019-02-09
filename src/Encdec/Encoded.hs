{-# LANGUAGE DeriveFunctor #-}

module Encdec.Encoded where

import HuskPrelude

newtype Encoded a =
  Encoded { encoded :: ByteString }
  deriving (Eq, Show, Functor)
