{-# LANGUAGE NoImplicitPrelude #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module D1.Util
  ( plus2
  ) where

import RIO

plus2 :: Int -> Int
plus2 = (+ 2)
