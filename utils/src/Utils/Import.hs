{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Import
  ( module RIO,
    module Utils.Types,
    search,
  )
where

import RIO
import Utils.Types

search ::
  forall partial solution.
  (partial -> Maybe solution) ->
  (partial -> [] partial) ->
  partial ->
  [] solution
search finished' refine' = search'
  where
    search' partial
      | Just sln <- finished' partial = pure sln
      | otherwise = refine' partial <&> search' & join
