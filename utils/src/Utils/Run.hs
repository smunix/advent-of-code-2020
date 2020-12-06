{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Run (run) where

import Utils.Import

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
