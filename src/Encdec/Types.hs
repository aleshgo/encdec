{-# LANGUAGE DeriveFunctor #-}

module Encdec.Types where

import HuskPrelude

newtype Encoded a =
  Encoded { encoded :: ByteString }
  deriving (Eq, Show, Functor)
