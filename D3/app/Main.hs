{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import D3.Import
import D3.Run
import Options.Applicative.Simple
import qualified Paths_D3
import RIO.Process

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_D3.version)
      "Header for command line arguments"
      "Program description, also for command line arguments"
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
      )
      empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
            }
     in runRIO app run
