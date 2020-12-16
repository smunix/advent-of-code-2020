{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module D8.Types where

import qualified Data.ByteString.Lazy.Char8 as StringC
import qualified Data.Kind as Ty
import RIO
import RIO.ByteString.Lazy (hGetContents)
import RIO.Process

-- | Command line arguments
newtype Options = Options
  { optionsVerbose :: Bool
  }

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options
    -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

class TestFile (m :: Ty.Type -> Ty.Type) where
  type TestFileAction m :: Ty.Type
  testFile :: TestFileAction m

instance () => TestFile (RIO env) where
  type
    TestFileAction (RIO env) =
      FilePath ->
      (StringC.ByteString -> RIO env ()) ->
      RIO env ()
  testFile fp action =
    withFile
      fp
      ReadMode
      (hGetContents >=> action)

ghci ::
  forall env opt.
  (TestFile (RIO env)) =>
  (LogFunc -> ProcessContext -> opt -> env) ->
  opt ->
  FilePath ->
  (StringC.ByteString -> RIO env ()) ->
  IO ()
ghci appC opt fp action = do
  lo <- logOptionsHandle stderr True
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = appC lf pc opt
     in runRIO app (testFile @(RIO env) fp action)
